# Idris2 Fmt — Project Memories

## Type-Hole Workflow (v3)
Strictly follow these 8 steps. Do not skip or combine.

1. **Select a type hole** from the codebase
2. **Query compiler:** `idris2 --repl idris2-fmt.ipkg <<'EOF'; :import <Module>; :t rhs_<name>; EOF` (NO `?` prefix!)
3. **Fill the hole.** If pattern matching needed, consult Idris's casesplit suggestion first
4. **Check compilation:** `rm -rf build/ttc/ && idris2 --build idris2-fmt.ipkg`
5. **Query new holes** via REPL to verify expected types match plan
6. **Compare types** — ensure compiler output matches implementation logic
7. **Commit** with detailed message referencing step numbers and hole types
8. **Repeat** until build is clean

### Hole Querying Rules
| Syntax | Result | Use? |
|--------|--------|------|
| `:t rhs_name` | Context + expected type | YES |
| `:t ?rhs_name` | Nested useless hole | NO |

Holes inside instance bodies (`Functor where ...`) are **NOT** visible to REPL. Only top-level function holes are queryable.

## Build Commands

```bash
# Build (uses prettier from flake.nix, do NOT install manually)
nix develop -c idris2 --build idris2-fmt.ipkg

# Check flake
nix flake check --no-build

# Run executable
./build/exec/idris2-fmt --help
```

**CRITICAL:** Do not install Idris2 packages manually to `~/.idris2/`. The flake
provides `prettier` via `idris2-withpkgs`. Manual installs shadow the Nix
store paths and cause version conflicts.

## Known Pitfalls

### 1. Code-gen bug: `where`-bound helpers in exported instances

**Symptom:** Chez backend crashes with:
```
attempt to reference unbound identifier IdrisFmtC-45Printer-opts-1318
```

**Cause:** `where`-bound helper functions inside exported `Pretty` instances
that capture the implicit `{opts : _}` parameter. Idris2's code generator fails
to thread the implicit through to the helper's closure.

**Fix:** Move all helpers into the `mutual` block as top-level functions.
They still capture `opts` implicitly, but the code generator handles it
correctly when they're sibling definitions rather than nested `where` clauses.

### 2. `interface` is a reserved keyword

Cannot use `interface` as a record field name. Use `interfaceName` instead.

### 3. `vappend` is non-associative

Chained `vappend` must be parenthesized:
```idris
(a `vappend` b) `vappend` c   -- OK
a `vappend` b `vappend` c     -- ERROR: non-associative
```

### 4. Type alias qualification

`import Data.List as L` does NOT make `L.List` work. `List` is defined in
`Prelude.Basics`, not `Data.List`. Use bare `List` everywhere.

### 5. `Doc.empty` ambiguity

When `Text.PrettyPrint.Bernardy` is imported, `Doc.empty` may be ambiguous
between `Doc.empty` and `Layout.empty`. Use explicit namespace or ensure the
type context resolves it. Inside functions with `{opts : _}` parameter,
`Doc.empty` usually resolves correctly.

### 6. Totality on mutual pretty-printers

`%default total` fails on mutually recursive `Pretty` instances for the AST.
Use `%default covering` on `Printer.idr` and `Main.idr`.

### 7. Linear config parameters

Do not annotate `Config` with linearity `(1 cfg : Config)` when the config is
used in the result type AND the function body. Idris2 rejects reusing a
linear variable in the return type after destructuring it.

### 8. Executable generation

Add `executable = idris2-fmt` to `.ipkg` to produce `build/exec/idris2-fmt`.

### 9. Type alias pattern matching (CRITICAL)

`RigCount` is a type alias for `ZeroOneOmega`. Idris2's pattern matcher
**does not** unify type aliases with their underlying type in pattern matches.

**Symptom:**
```
Mismatch between: RigCount and ZeroOneOmega.
```

**Fix:** Use `elimSemi` from `Algebra.Semiring` instead of pattern matching:
```idris
translateRig c = elimSemi AST.Rig0 AST.Rig1 (const AST.RigW) c
```

### 10. Adding compiler API dependency

To use Idris2 compiler parser in the flake:

```nix
idrisLibraries = with idris2-withpkgs.packages.${system}; [
  prettier
  idris2api   # or idris2
];
```

In `.ipkg`, add `depends = idris2` (NOT `idris2api`). The package name is
`idris2` even though the flake output is `idris2api`.

### 11. EComment constructor arity

`EComment` takes **two** arguments, not one:
```idris
EComment : C.Comment -> Expr nm -> Expr nm   -- comment + fallback expr
```

### 12. OpStr carries Name, not String

In Idris2's compiler AST, `OpSymbols` carries a `Name`, not a `String`:
```idris
data OpStr' nm = OpSymbols nm | Backticked nm
```

For the formatter, convert with `show n`.

### 13. Namespace MkNS is private

`Namespace` constructor `MkNS` is not exported. Use `show ns` to get a string
representation, or extract the list through the `nsToList` helper (if available).

### 14. PDef clause name extraction

To translate `PDef` (function definition), extract the function name from the
LHS of the **first** clause using a recursive `getFnName` helper that traverses
`PApp`, `PNamedApp`, `PAutoApp`, and `PBracketed` to find the `PRef`.

### 15. Primitive types in PPrimVal

`PPrimVal` carries a `Constant` which can be:
- Literal values: `I Int`, `BI Integer`, `Str String`, `Ch Char`, `Db Double`
- Type references: `PrT PrimType` (e.g., `IntType`, `StringType`)

The `PrT` case must be handled to print `Int -> Int` correctly (not `0 -> Int`).

### 16. `case` expressions inside `let` with `Doc` type

**Symptom:**
```
Can't solve constraint between: ?opts [locals in scope: ...] and opts
```

**Cause:** `case` expressions inside `let` bindings with `Doc opts` type
introduce fresh implicit `opts` variables that don't unify with the outer scope.

**Fix:** Use `if ... then ... else ...` instead of `case` for `Doc`-valued
expressions, or lift the logic to top-level pattern matching:
```idris
-- BAD:
let x = case y of Nothing => empty; Just a => line a
-- GOOD:
let x = maybe empty line y

-- BAD:
let header = case params of [] => ...; _ => ...
-- GOOD:
let header = if null params then ... else ...
```

### 17. `<++>` adds space even with `empty` left operand

**Symptom:** Leading spaces in output when the left side of `<++>` is `empty`.

**Cause:** `x <++> y = x <+> space <+> y`. If `x = empty`, the result starts
with a space.

**Fix:** Use conditional composition or `hsep` with filtering:
```idris
-- BAD:
hsep [] <++> pretty n   -- produces " n"
-- GOOD:
case fnOpts of
  [] => pretty n <++> colon <++> pretty ty
  _  => hsep (map fnOptDoc fnOpts) <++> pretty n <++> colon <++> pretty ty
```

### 18. `empty `vappend` x` produces leading blank lines

**Symptom:** Blank lines before first declaration or between declarations.

**Cause:** `vsep [] = empty`, and `empty `vappend` x = flush empty <+> x`.
`flush empty` on `empty` Layout produces `[<"", ""]`, which `unlines` renders
as a leading blank line.

**Fix:** Avoid `vsep [] `vappend` x`. Check for empty lists first:
```idris
-- BAD:
vsep (map pretty comments) `vappend` body   -- when comments = []
-- GOOD:
case comments of
  [] => body
  _  => vsep (map pretty comments) `vappend` body
```

### 19. `putStrLn` doubles trailing newline

**Symptom:** Trailing blank line at end of output.

**Cause:** `Doc.render` uses `unlines` which adds a trailing `\n`. `putStrLn`
adds another `\n`.

**Fix:** Use `putStr` for stdout output; `render` already ends with `\n`.

### 20. `val` ambiguity between `WithData` and `WithBounds`

**Symptom:** Compiler can't resolve `val` when working with `WithFC`, `WithDoc`,
etc. from `Core.WithData`.

**Fix:** Hide the conflicting `val`:
```idris
%hide Libraries.Text.Bounded.WithBounds.val
```

### 21. `WithData` projections overload resolution

**Symptom:** Compiler tries many `.names` / `.rig` / etc. overloads and fails.

**Fix:** Use explicit `WithData.get "fieldname"` instead of `.fieldname`, or
pattern match on constructors where possible. For `BasicMultiBinder'`, pattern
match on `MkBasicMultiBinder` to access fields directly.

### 22. `toPath` vs `unsafeUnfoldModuleIdent` for module paths

**Symptom:** Import paths like `List Data` (reversed) or `Data/List` (wrong
separator).

**Fix:** `toPath` returns file-system-style paths with `/` separator.
Split on `/` and join with `.` for printing:
```idris
translateImport imp =
  let path = splitString '/' (toPath imp.path)
   in MkImportDecl imp.reexport path alias Nothing Nothing
```
And in printer, join with `.`:
```idris
line (concat (intersperse "." name))
```

### 23. `PInfer` for implicit parameter types

**Symptom:** Interface parameters show as `(a : ?unsupported_?)`.

**Cause:** Unannotated interface parameters are parsed as `PInfer _` (infer
the type), not `PType`.

**Fix:** Handle `PInfer` in `translatePTerm`:
```idris
translatePTerm (PInfer _) = AST.EImplicit
```

### 24. Idris2 `mutual` block forward reference limitations

**Symptom:** Functions inside a `mutual` block cannot reference top-level
functions defined later in the file. Error: "Undefined name X".

**Cause:** Idris2 resolves forward references for top-level functions, but
functions inside a `mutual` block can only see:
- Other functions inside the same `mutual` block (regardless of order)
- Top-level functions defined BEFORE the `mutual` block

They CANNOT see top-level functions defined AFTER the `mutual` block.

**Fix:** Define helper functions BEFORE the `mutual` block, or pass the
recursively-used function as an explicit argument (higher-order function):
```idris
-- BEFORE mutual block:
translatePDo_ : (IS.PTerm -> AST.Expr AST.Name) -> IS.PDo' CN.Name -> AST.DoStmt AST.Name
translatePDo_ trans (DoExp _ tm) = AST.DoExp (trans tm)

-- Inside mutual block:
translatePTerm (PDoBlock _ ns stmts) =
  AST.EDo (map show ns) (map (translatePDo_ translatePTerm) stmts)
```

### 25. `mutual` block: function definitions inside pattern matching

**Symptom:** "No type declaration for X" when defining a function inside a
`mutual` block that appears between clauses of another function.

**Cause:** Idris2's parser continues pattern matching for the current function
until it encounters a clause that doesn't match. A new function definition
(with its own type signature) in the middle is treated as a pattern match
attempt and fails.

**Fix:** Never define a new function between clauses of a pattern-matched
function within a `mutual` block. Place all helper functions either:
- Before the pattern-matched function starts, or
- After it ends (with a blank line separating them)

## Architecture

- **Pipeline:** `parseModule` >=> `transformModule` >=> `printModule`
- **Fusion point:** `formatSource` composes all passes with `map` and function
  composition — no intermediate structures materialized.
- **Transform passes:** `sortImports` (stable, only sorts contiguous import
  blocks) and `mergeBlankLines` (collapses consecutive `DBlank` into one).
- **Printer strategy:** Implement `Pretty` interface from `prettier` for every
  AST type. Precedence-aware via `prettyPrec`. Layout decisions delegated to
  `prettier`'s optimal layout engine (`<|>`, `ifMultiline`, `hang`, `sep`).
- **Parser bridge:** Calls `Parser.Source.runParser` with `Idris.Parser.prog`.
  Translates compiler AST (`PTerm`, `PDecl`, `PClause`) to formatter AST.

## Current Status (v0.14.0)

**All critical formatting bugs fixed:**
- `PRef` operator parenthesization in function clauses (`(<*>) af ax = ...`)
- `PClaim` operator parenthesization in type signatures
- `PRef (UN (Field _))` accessor sections (`(.task)`)
- `PPostfixAppPartial` bare field access
- `DoBindPat` alternatives rendering (`| Nothing => ...`)
- `PLocal` declarations rendering (`let ... in`)
- `NewPi` (forall) rendering
- Record parameter spurious parens (`record Parser a` not `Parser (a)`)

**Test coverage:**
- 31 reference tests (compile + format + idempotency + convergence)
- taiga-cli end-to-end integration (55 modules, zero manual fixes)

**Tagged:** v0.14.0

## Next Steps

1. **Restore totality:** Switch modules from `%default covering` to `%default total`
2. **Attach comments:** Extract comment annotations from parser `State.decorations`
3. **Round-trip tests:** Parse -> Format -> Parse should yield equivalent AST
