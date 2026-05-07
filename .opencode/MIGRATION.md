# idris2-fmt Architecture Migration Plan

## Problem

The current formatter uses an intermediate AST (`IdrisFmt.AST`) that is a lossy
subset of Idris2's internal `PTerm`/`PDecl` types. Every Idris2 language
construct must be manually translated from `PTerm` to our AST in
`Parser.idr`, then manually printed back to source in `Printer.idr`.

This creates a permanent bug surface. Recent regressions (v0.11.x series):

| Bug | Root cause | Status |
|-----|-----------|--------|
| `PDPair` mangled to `?unsupported_...` | Missing `PDPair` case in `translatePTerm` | Fixed v0.11.4 |
| `PSnocList` dropped | Missing `PSnocList` case in `translatePTerm` | Fixed v0.11.4 |
| `PQuoteName`/`PQuoteDecl` dropped | Missing cases in `translatePTerm` | Fixed v0.11.4 |
| `=>` replaced by `->` in constraints | `AutoImplicit` arrow logic incomplete | Fixed v0.11.5 |
| `parameters {auto ...}` → `(env : Type)` | `PiInfo`/`RigCount` lost in `DParams` AST | Fixed v0.11.6 |
| `parameters` got spurious `where` | DParams printer wrapped params in parens | Fixed v0.11.6 |

Each fix requires: AST extension + parser case + N printer cases (for each
printer variant). This is unsustainable as Idris2 syntax evolves.

## Target Architecture

Print directly from Idris2's `PTerm`/`PDecl` types, bypassing our custom AST.

```
Source → Idris2 Parser → Module (List PDecl) → IdrisFmt.Printer → Source
                ↑                              ↑
                └── extract comments from FC ──┘
```

Idris2 already has a reference pretty-printer (`Idris.Pretty.idr`) that renders
`PTerm` back to valid Idris2 source. We replace our custom AST with Idris2's
types and build our configurable layout engine on top of that existing printer
logic.

## Migration Steps

### Phase 0: Preparation (1 day)

1. **Audit Idris2.Pretty.idr**
   - Read `/srv/ai/libs_sources/Idris2/src/Idris/Pretty.idr`
   - Understand how it handles layout choices (horizontal/vertical, alignment)
   - Identify which layout decisions are hardcoded vs configurable

2. **Audit comment handling**
   - Comments in Idris2 are attached to `FC` (file context) metadata as
     `WithComments` wrappers
   - Document how to extract comment text from source positions
   - Design comment interleaving strategy

3. **Freeze current test suite**
   - Ensure all Reference/Broken tests compile and pass
   - These tests define the acceptance criteria for the migration

### Phase 1: Core Types (1-2 days)

1. **Create new printer module hierarchy**
   ```
   src/IdrisFmt/Printer/
     Core.idr          -- Pretty-print directly from PTerm
     Layout.idr        -- Layout engine (existing logic, new types)
     Configurable.idr  -- Config-driven layout choices
     Complete.idr      -- Monadic printer with traces
   ```

2. **Write `Core.idr`**
   - Import `Idris.Syntax` types directly
   - Write `prettyPTerm : PTerm -> Doc` using `Idris.Pretty` as reference
   - Write `prettyPDecl : PDecl -> Doc`
   - Handle ALL PTerm constructors (no catch-all fallbacks)
   - Each constructor maps to exactly one rendering rule

3. **Comment extraction**
   - `extractComments : String -> SortedMap (Int, Int) Comment`
   - Maps source positions to comment text
   - Used by printer to interleave comments at correct positions

### Phase 2: Layout Engine (1-2 days)

1. **Port existing layout logic**
   - `Blocks.idr` flattening logic (EPi chains, ELet chains)
   - `Align.idr` post-processing alignment pass
   - `Measure.idr` width calculations
   - All operate on `Doc` output, not AST — mostly unchanged

2. **Configuration integration**
   - `ArrowStyle` (Trailing/Leading)
   - `LetStyle` (Inline/Auto/Block)
   - `IfStyle` (Compact/Indented)
   - Inject config decisions at specific `PTerm` patterns
   - Example: `PPi` chain → choose arrow style
   - Example: `PLet` → choose let style

### Phase 3: Integration (1 day)

1. **Replace `Main.idr` pipeline**
   - Old: `parse → translatePTerm → AST → pretty → output`
   - New: `parse → Module → prettyModule → output`

2. **Delete dead code**
   - Remove `IdrisFmt.AST.idr`
   - Remove `translatePTerm`, `translatePDecl` from `Parser.idr`
   - Remove old `Printer.idr`, `Printer/Complete.idr`, `Printer/Configurable.idr`
   - Keep `Comments.idr`, `Config.idr`, `Align.idr`, `Measure.idr`, `Layout.idr`

### Phase 4: Testing & Validation (1-2 days)

1. **Run full test suite**
   - All existing Reference/Broken tests must pass
   - Format → compile must succeed for all
   - Idempotency and convergence checks must pass

2. **Add edge-case tests**
   - The bug cases from v0.11.x become permanent regression tests
   - Add new tests for any Idris2 features not yet covered

3. **Performance check**
   - Format a large codebase (e.g., idris2-http)
   - Ensure no significant slowdown vs v0.11.6

## Risks & Mitigations

| Risk | Likelihood | Impact | Mitigation |
|------|-----------|--------|------------|
| Comment interleaving is complex | High | High | Start with simple strategy: attach comments to nearest AST node via FC position. Iterate. |
| Idris2.Pretty has incompatible layout choices | Medium | Medium | Don't use `Idris.Pretty` directly — use its constructor handling as reference, write our own layout logic on `PTerm`. |
| Performance regression from position tracking | Low | Low | Comments are extracted once at parse time; printer does O(1) lookup per node. |
| Breaking change for users with custom plugins | Low | High | This is an internal refactor — CLI interface unchanged. |

## Acceptance Criteria

- [ ] All 20 existing test cases pass (compile → format → compile)
- [ ] v0.11.x regression tests added and passing
- [ ] Format idempotency: `format(format(x)) == format(x)`
- [ ] Format convergence: `format(broken) == reference`
- [ ] Performance within 10% of v0.11.6 on large codebase
- [ ] No new AST constructors needed for 6 months after migration

## Timeline

| Phase | Days | Deliverable |
|-------|------|-------------|
| 0 | 1 | Audit report, comment strategy doc |
| 1 | 1-2 | `Printer/Core.idr` with all PTerm constructors |
| 2 | 1-2 | Layout engine + config integration |
| 3 | 1 | Pipeline integration, dead code removal |
| 4 | 1-2 | Tests pass, performance validated |
| **Total** | **5-8 days** | **v0.12.0** |

## Decision Log

**2025-05-07:** Decision to migrate. Custom AST approach has proven
unsustainable after 6 bugs in 3 patch releases (v0.11.4-v0.11.6). Each fix
creates more complexity. Direct PTerm printing eliminates the translation
layer entirely.

**Rejected alternative:** Extend custom AST to be exhaustive. This would
require replicating ~40 PTerm constructors × 3 printer variants = 120 cases
minimum, plus ongoing maintenance as Idris2 evolves. False economy.
