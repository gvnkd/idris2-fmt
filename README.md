# idris2-fmt

A production-ready source code formatter for [Idris 2](https://idris-lang.org/), built on the compiler's own parser and a custom pretty-printing engine with post-processing alignment.

## Features

- **Compiler-native parsing** — Uses Idris 2's built-in parser (`Idris.Parser`), not a custom lexer/parser. This guarantees 100% syntactic compatibility with the language.
- **Pretty-printing with `prettier`** — Layout engine via `Text.PrettyPrint.Bernardy` (the `prettier` package), with configurable line width and indentation.
- **Post-processing alignment** — After rendering, a string-level alignment pass aligns:
  - Type signature colons (`:`)
  - Function definition equals signs (`=`)
  - Case alternative arrows (`=>`)
  - Record field colons (`:`)
- **Comment preservation** — Both line comments (`--`) and block comments (`{- -}`) are preserved. Doc comments (`|||`) are extracted and re-attached to declarations.
- **Where clause support** — Local definitions in `where` blocks are parsed, stored, and printed correctly.
- **Import sorting** — Imports are sorted alphabetically and deduplicated.
- **Blank line insertion** — Declarations are separated by blank lines based on source line gaps.
- **Self-hosting** — The formatter can format its own source code and compile successfully.

## Installation

### Prerequisites

- [Nix](https://nixos.org/) with flakes enabled
- Idris 2 (provided via the Nix flake)

### Building

```bash
# Enter the development shell
nix develop

# Build the formatter
idris2 --build idris2-fmt.ipkg

# The executable will be at:
# ./build/exec/idris2-fmt
```

### Nix build (without entering devShell)

```bash
nix build
# Result symlinked to ./result
```

## Usage

```bash
# Format a file and print to stdout
idris2-fmt MyModule.idr

# Format multiple files
idris2-fmt File1.idr File2.idr File3.idr

# Format in place (overwrite files)
idris2-fmt --inplace MyModule.idr

# Check formatting without writing (exit 1 if needs formatting)
idris2-fmt --check MyModule.idr

# Read from stdin, write to stdout
echo 'module Main\nmain = putStrLn "hello"' | idris2-fmt --stdin

# Custom indentation and line width
idris2-fmt --indent 4 --width 100 MyModule.idr
```

### Exit codes

| Mode | Exit 0 | Exit 1 |
|------|--------|--------|
| Default | Success | Parse/format error |
| `--check` | Already formatted | Needs formatting |

## Architecture

### Pipeline

```
Source Text
    |
    v
[Idris 2 Parser] --(PDecl)--> [Parser Bridge] --(AST)--> [Transform]
                                                               |
                                                               v
[Align] <--(String)-- [Render] <--(Doc)-- [Printer] <--(AST)--+
    |
    v
Formatted Text
```

### Components

| Module | Responsibility |
|--------|----------------|
| `IdrisFmt.AST` | GADT-based AST parameterized by `nm` (name type). Supports `Name`, `Expr`, `Decl`, `Clause`, and all Idris 2 constructs. |
| `IdrisFmt.Parser` | Bridges compiler `PDecl`/`PTerm` to formatter `AST`. Extracts comments from `State.decorations`, handles visibility, where blocks, and all `PTerm` constructors. |
| `IdrisFmt.Printer` | Pretty-prints AST to `Doc` using `prettier`. Handles precedence, indentation, and layout choices (horizontal vs vertical). |
| `IdrisFmt.Align` | Post-processes rendered string to align tokens (`:`, `=`, `=>`) in consecutive lines at the same indentation level. |
| `IdrisFmt.Transform` | Sorts imports, merges blank lines, and other AST-level transformations. |
| `IdrisFmt.Config` | Configuration record: `indentWidth`, `lineLength`, `alignRules`. |
| `IdrisFmt.Comments` | Comment style definitions (`LineComment`, `BlockComment`, `DocComment`) and extraction helpers. |
| `IdrisFmt.Doc` | Document helpers: `ident`, `keyword`, `operator_`. |
| `IdrisFmt.CLI` | Argument parsing for `--check`, `--inplace`, `--stdin`, `--indent`, `--width`, `--help`. |

### AST Design

The AST uses GADTs with a name parameter:

```idris
data Expr : Type -> Type where
  ERef    : nm -> Expr nm
  EPi     : RigCount -> PiInfo (Expr nm) -> Maybe nm -> Expr nm -> Expr nm -> Expr nm
  ELam    : RigCount -> PiInfo (Expr nm) -> Expr nm -> Expr nm -> Expr nm -> Expr nm
  ELet    : RigCount -> Expr nm -> Expr nm -> Expr nm -> Expr nm -> List (Clause nm) -> Expr nm
  EApp    : Expr nm -> Expr nm -> Expr nm
  ECase   : Expr nm -> List (Clause nm) -> Expr nm
  ELocal  : List (Decl nm) -> Expr nm -> Expr nm
  EForall : List nm -> Expr nm -> Expr nm
  -- ... and 30+ more constructors
```

Key design decisions:
- `Name` has `UN`, `MN`, and `NS` constructors to preserve module qualifiers.
- `Clause` carries `where` block declarations in `MkClause`.
- `Comment` is threaded through `EComment` nodes.

## Alignment Rules

Alignment is applied as a post-processing pass on the rendered output. Consecutive lines at the same indentation level containing the target token are padded so the token aligns vertically.

| Rule | Token | Default | Description |
|------|-------|---------|-------------|
| `alignTypeSigs` | ` : ` | `True` | Align colons in type signatures and data constructors |
| `alignFunctionDefs` | ` = ` | `True` | Align equals signs in adjacent function clauses |
| `alignCaseArrows` | ` => ` | `True` | Align fat arrows in `case` alternatives |
| `alignRecordFields` | ` : ` | `True` | Align colons in record field declarations |

Example:

```idris
foo : Int -> Int
bar : String -> String

map f []        = []
map f (x :: xs) = f x :: map f xs

case xs of
  []    => 0
  x::xs => 1 + length xs
```

## Self-Hosting

`idris2-fmt` can format its own source code. After formatting all `src/` files, the project compiles successfully and all 16 tests pass.

```bash
# Format all source files
for f in src/IdrisFmt/*.idr src/Main.idr; do
  ./build/exec/idris2-fmt --inplace "$f"
done

# Verify compilation
idris2 --build idris2-fmt.ipkg

# Run tests
cd tests
idris2 --build tests.ipkg
./build/test/exec/runtests $(realpath ../build/exec/idris2-fmt)
```

## Testing

The test suite uses the `test` package (Idris 2's built-in test framework) with golden file comparison.

```bash
# Run all tests
nix develop -c run-tests

# Or manually:
cd tests
idris2 --build tests.ipkg
./build/test/exec/runtests $(realpath ../build/exec/idris2-fmt)
```

### Test coverage

- `simple` — Basic declarations
- `functions` — Function definitions and clauses
- `data_types` — `data` declarations with constructors
- `records` — `record` declarations
- `interfaces` — `interface` and `implementation`
- `imports` — Import sorting and deduplication
- `case` — `case` expressions
- `do_blocks` — `do` notation
- `mutual` — `mutual` blocks
- `parameters` — `parameters` blocks
- `using` — `using` blocks
- `namespace` — `namespace` blocks
- `directives` — `%default`, `%hide`, etc.
- `builtin` — Built-in function handling
- `comments` — Line and block comment preservation
- `doc_comments` — `|||` doc comment preservation

## Development

### Code style

See `STYLE.md` for the formatting rules enforced by `idris2-fmt`.

### Type-hole workflow

All Idris 2 code follows the type-hole workflow:

1. Write signatures with type holes: `func args = ?rhs_func`
2. Compile immediately, read hole types
3. Fill holes one at a time, compile after each
4. Prefer functor/applicative/fold over loops

### Known limitations

- `where` clauses are supported but very deeply nested `where` blocks may need manual review.
- Some advanced TTImp constructs (e.g., `PRunElabDecl` bodies) are translated to placeholder comments.
- Multiline strings are preserved as-is.

## License

This project is provided as-is for educational and practical use. See the repository for licensing details.

---

Built with Nix, Idris 2, and `prettier`.
