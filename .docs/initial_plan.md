# Idris2 Code Formatter — Initial Design Plan

## 1. Overview

This document describes the architecture and Phase-1 skeleton for `idris2-fmt`, a source-code formatter for Idris2. The design follows functional programming best practices: functors and applicatives for composition, function fusion to eliminate intermediate allocations, GADTs for the core AST, and totality-by-default. The skeleton is fully type-checked but leaves implementation holes (`?rhs_...`) as clear integration points for Phase 1.

## 2. High-Level Architecture

The formatter is a pure pipeline:

```
Source Text
    |
    v  [Parser.parseModule]
Parsed AST (with comments)
    |
    v  [Transform.transformModule]
Normalized AST
    |
    v  [Printer.printModule / Pretty instances]
Pretty-printing Document (Doc opts)
    |
    v  [Doc.renderDoc]
Formatted Text
```

### 2.1 Call Diagram

```
Main.main
  |
  +-- CLI.parseArgs
  |
  +-- Main.run
        |
        +-- Main.processFile
        |     |
        |     +-- System.File.readFile
        |     +-- Main.formatSource
        |     |     |
        |     |     +-- Parser.parseModule ---------------+
        |     |     |                                       |
        |     |     +-- Transform.transformModule           |
        |     |     |     |                                 |
        |     |     |     +-- Transform.sortImports         |
        |     |     |                                       |
        |     |     +-- Printer.printModule                 |
        |     |     |     |                                 |
        |     |     |     +-- Pretty (Decl Name)            |
        |     |     |     |     |                           |
        |     |     |     |     +-- Pretty (Expr Name) <----+
        |     |     |     |     |                           |
        |     |     |     |     +-- Pretty (Clause Name)    |
        |     |     |     |     |                           |
        |     |     |     |     +-- Pretty (DoStmt Name)    |
        |     |     |     |                                 |
        |     |     |     +-- Pretty (DataDecl Name)        |
        |     |     |     |                                 |
        |     |     |     +-- Pretty (RecordDecl Name)      |
        |     |     |     |                                 |
        |     |     |     +-- Pretty (InterfaceDecl Name)   |
        |     |     |     |                                 |
        |     |     |     +-- Pretty (ImplDecl Name)        |
        |     |     |                                       |
        |     |     +-- Doc.renderDoc                       |
        |     |                                               |
        |     +-- System.File.writeFile (or diff in check mode)
        |
        +-- (loop over remaining files)
```

## 3. GADT Design

The core AST is a family of types parameterized by the name representation `nm : Type`. This allows future passes to change the name type (e.g., from raw `String` names to resolved `Name` objects) without altering the tree shape.

```
Decl : Type -> Type
  |
  +-- DModule      : String -> List String -> Decl nm
  +-- DImport      : ImportDecl -> Decl nm
  +-- DClaim       : List Comment -> nm -> Expr nm -> List FnOpt -> Decl nm
  +-- DDef         : List Comment -> nm -> List (Clause nm) -> Decl nm
  +-- DData        : List Comment -> DataDecl nm -> Decl nm
  +-- DRecord      : List Comment -> RecordDecl nm -> Decl nm
  +-- DInterface   : List Comment -> InterfaceDecl nm -> Decl nm
  +-- DImpl        : List Comment -> ImplDecl nm -> Decl nm
  +-- DFixity      : FixityDecl -> Decl nm
  +-- DNamespace   : List String -> List (Decl nm) -> Decl nm
  +-- DMutual      : List (Decl nm) -> Decl nm
  +-- DParams      : List (nm, Maybe (Expr nm)) -> List (Decl nm) -> Decl nm
  +-- DUsing       : List (Maybe nm, Expr nm) -> List (Decl nm) -> Decl nm
  +-- DComment     : Comment -> Decl nm
  +-- DBlank       : Nat -> Decl nm

Expr : Type -> Type
  |
  +-- ERef        : nm -> Expr nm
  +-- EPi         : RigCount -> PiInfo (Expr nm) -> Maybe nm -> Expr nm -> Expr nm -> Expr nm
  +-- ELam        : RigCount -> PiInfo (Expr nm) -> Expr nm -> Expr nm -> Expr nm -> Expr nm
  +-- ELet        : RigCount -> Expr nm -> Expr nm -> Expr nm -> Expr nm -> List (Clause nm) -> Expr nm
  +-- EApp        : Expr nm -> Expr nm -> Expr nm
  +-- ENamedApp   : Expr nm -> nm -> Expr nm -> Expr nm
  +-- EAutoApp    : Expr nm -> Expr nm -> Expr nm
  +-- EDelayed    : Expr nm -> Expr nm
  +-- EDelay      : Expr nm -> Expr nm
  +-- EForce      : Expr nm -> Expr nm
  +-- ECase       : Expr nm -> List (Clause nm) -> Expr nm
  +-- ELocal      : List (Decl nm) -> Expr nm -> Expr nm
  +-- EList       : List (Expr nm) -> Expr nm
  +-- ESnocList   : SnocList (Expr nm) -> Expr nm
  +-- EPair       : Expr nm -> Expr nm -> Expr nm
  +-- EString     : List (StringPart nm) -> Expr nm
  +-- EDo         : Maybe String -> List (DoStmt nm) -> Expr nm
  +-- EIdiom      : Maybe String -> Expr nm -> Expr nm
  +-- EIf         : Expr nm -> Expr nm -> Expr nm -> Expr nm
  +-- EHole       : String -> Expr nm
  +-- EType       : Expr nm
  +-- EImplicit   : Expr nm
  +-- EQuote      : Expr nm -> Expr nm
  +-- EUnquote    : Expr nm -> Expr nm
  +-- EPrim       : Constant -> Expr nm
  +-- EOp         : Expr nm -> OpStr nm -> Expr nm -> Expr nm
  +-- EPrefixOp   : OpStr nm -> Expr nm -> Expr nm
  +-- ESectionL   : OpStr nm -> Expr nm -> Expr nm
  +-- ESectionR   : Expr nm -> OpStr nm -> Expr nm
  +-- EBracketed  : Expr nm -> Expr nm
  +-- EAs         : nm -> Expr nm -> Expr nm
  +-- EDotted     : Expr nm -> Expr nm
  +-- EComment    : Comment -> Expr nm -> Expr nm

Clause : Type -> Type
  |
  +-- MkClause  : Expr nm -> Expr nm -> Clause nm
  +-- MkWith    : Expr nm -> List (Expr nm) -> List (Clause nm) -> Clause nm
  +-- MkImposs  : Expr nm -> Clause nm

DoStmt : Type -> Type
  |
  +-- DoExp     : Expr nm -> DoStmt nm
  +-- DoBind    : nm -> RigCount -> Maybe (Expr nm) -> Expr nm -> DoStmt nm
  +-- DoBindPat : Expr nm -> Maybe (Expr nm) -> Expr nm -> List (Clause nm) -> DoStmt nm
  +-- DoLet     : nm -> RigCount -> Expr nm -> DoStmt nm
  +-- DoLetPat  : Expr nm -> Expr nm -> List (Clause nm) -> DoStmt nm
  +-- DoRewrite : Expr nm -> DoStmt nm

Record types (DataDecl, RecordDecl, InterfaceDecl, ImplDecl, ConDecl, FieldDecl)
are standard Idris2 records grouping the relevant fields.
```

## 4. Module Descriptions

### IdrisFmt.Config
Central configuration record storing `indentWidth` and `lineLength`. Future phases will add alignment rules, import grouping, and pragma handling.

### IdrisFmt.Comments
Comment representation: style (`LineComment` | `BlockComment`), content string, and source position (`line`, `col`). In Phase 1 comments are preserved as first-class AST nodes (`DComment`, `EComment`). Phase 2 will move to per-node annotations for finer-grained placement control.

### IdrisFmt.AST
The full formatter AST, parameterized over `nm`. Uses mutual recursion for `Decl`, `Expr`, `Clause`, `DoStmt`, and auxiliary types. Records group related fields (`DataDecl`, `RecordDecl`, `InterfaceDecl`, `ImplDecl`).

### IdrisFmt.Doc
Thin, qualified wrapper over `prettier` (`Text.PrettyPrint.Bernardy`). Responsibilities:
- Convert `Config` to `LayoutOpts`
- Provide formatter-specific helpers: `keyword`, `operator_`, `ident`, `stringLit`
- Provide `renderDoc` to avoid namespace ambiguity between `Layout.render` and `Doc.render`

### IdrisFmt.Parser
Placeholder parsing interface. In Phase 1 all functions are type holes. The intended integration path is:
1. Call Idris2's `Parser.Source.runParser`
2. Extract `PTerm` / `PDecl` from the result
3. Translate compiler AST to `IdrisFmt.AST`
4. Attach comments from parser `State.decorations`

### IdrisFmt.Printer
Core formatting engine. Instead of monolithic `printExpr` functions, we implement the `Pretty` interface from `prettier` for every AST type. Benefits:
- Precedence-aware rendering via `prettyPrec`
- Reuse of `prettier`'s optimal layout engine (`<|>`, `ifMultiline`, `hang`, `sep`, etc.)
- Compositional: `pretty x <++> pretty y` works for any `Pretty` types

Mutual `Pretty` instances handle the mutually recursive AST.

### IdrisFmt.Transform
AST-to-AST transformations applied before printing. Phase 1 defines:
- `transformModule`: the fusion point composing all passes
- `sortImports`: alphabetize import declarations (non-imports stay in place)

Future passes: explicit-import expansion, unused-import removal, blank-line normalization.

### IdrisFmt.CLI
Argument parsing and usage text. Returns `Args` record on success, `Nothing` on invalid input.

### Main
Entry point. `formatSource` fuses the entire pipeline:
```idris
map (render opts . printModule cfg . transformModule cfg) . parseModule
```
No intermediate structures are materialized between the transformer and the printer.

## 5. Fusion Strategy

The formatter avoids explicit `for` loops and intermediate list materialization:

| Stage | Fusion Pattern |
|-------|----------------|
| Parser -> Transformer | `map transformModule . parseModule` (functor fusion) |
| Transformer -> Printer | `printModule cfg . transformModule cfg` (function composition) |
| Printer -> Renderer | `render opts . doc` (single traversal) |
| List of decls | `foldl vappend` or `vsep` from `prettier` (fold fusion) |

All list operations in `Transform` will be written as `map` / `filter` / `fold` compositions rather than recursive functions with explicit `cons`.

## 6. QTT (Quantitative Type Theory) Usage

- **Linear `Config` in `toLayoutOpts`**: `(1 cfg : Config) -> LayoutOpts`. The config is destructured exactly once; after conversion, the raw `LayoutOpts` is used for rendering.
- **Erased proofs on `Config`**: Future phase will add `{auto 0 lineLengthPositive : lineLength > 0}` to ensure line length is never zero, with the proof erased at runtime.
- **Erased module docstrings**: `DModule` may carry a `(0 doc : String)` in future phases if the docstring is only needed for metadata extraction, not runtime formatting.

## 7. Phase 1 Roadmap

1. **Type skeleton** (this plan): All modules type-check with holes.
2. **AST hardening**: Add missing constructors (pragma, builtin, elaboration, etc.).
3. **Parser bridge**: Implement `parseModule` by calling the Idris2 compiler parser and translating to our AST.
4. **Printer baseline**: Implement `Pretty` instances for `Expr` and `Decl`.
5. **Round-trip tests**: Parse -> Format -> Parse should yield equivalent AST.
6. **CLI integration**: Wire up file I/O, `--check`, and `--inplace`.
7. **Totality restoration**: Fill all holes and switch every module back to `%default total`.

## 8. Dependencies

- `prettier` (`Text.PrettyPrint.Bernardy`): Optimal-layout pretty-printing engine.
- Future: `parser` (from the idris2-packages registry) if we choose to write a standalone parser instead of reusing the compiler's.
