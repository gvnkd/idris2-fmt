# idris2-fmt Style Guide

This document describes the formatting rules enforced by `idris2-fmt`. The formatter is the single source of truth for style — run it on your code and the output is correct by definition.

## Usage

```bash
# Format in place
idris2-fmt --inplace MyModule.idr

# Check formatting (CI)
idris2-fmt --check MyModule.idr
```

## Global Options

| Flag | Default | Description |
|------|---------|-------------|
| `--indent N` | `2` | Number of spaces per indentation level |
| `--width N` | `80` | Target line length for layout engine |

## Layout Rules

The monadic printer (`IdrisFmt.Printer.Complete`) uses a configurable RWS monad to decide layout. Current tunable rules:

| Rule | Options | Default | Description |
|------|---------|---------|-------------|
| `LetStyle` | `Inline` \| `Auto` \| `Block` | `Auto` | Multi-binding `let` layout |
| `ArrowStyle` | `Trailing` \| `Leading` | `Trailing` | Function arrows in type signatures |
| `IfStyle` | `Compact` \| `Indented` | `Compact` | `if-then-else` layout |

### Let Styles

**Inline** (single binding, one line):
```idris
foo = let x = 1 in x + 1
```

**Block** (multiple bindings, aligned):
```idris
foo =
  let
    x = 1
    y = 2
  in x + y
```

### Arrow Styles

**Trailing** (arrow at end of line):
```idris
foo : Int -> String -> Bool
```

**Leading** (arrow on continuation line):
```idris
foo : Int
  -> String
  -> Bool
```

### If Styles

**Compact** (horizontal if it fits, vertical otherwise):
```idris
foo x = if x > 0 then "pos" else "non-pos"
```

**Indented** (always vertical):
```idris
foo x =
  if x > 0
    then "pos"
    else "non-pos"
```

## Alignment Rules

Alignment is applied as a post-processing pass on the rendered output. It groups consecutive lines at the same indentation level that contain the target token and pads shorter lines so that the token aligns vertically.

| Rule | Token | Default | Description |
|------|-------|---------|-------------|
| `alignTypeSigs` | ` : ` | `True` | Align colons in type signatures and data constructors |
| `alignFunctionDefs` | ` = ` | `True` | Align equals signs in adjacent function clauses |
| `alignCaseArrows` | ` => ` | `True` | Align fat arrows in `case` alternatives |
| `alignRecordFields` | ` : ` | `True` | Align colons in record field declarations |
| `alignListValues` | — | `False` | *(Reserved)* Align values in multi-line lists |

### Example: Type Signatures

```idris
foo : Int -> Int
bar : String -> String
```

### Example: Function Clauses

```idris
map f []        = []
map f (x :: xs) = f x :: map f xs
```

### Example: Case Alternatives

```idris
case xs of
  []    => 0
  x::xs => 1 + length xs
```

## Blank Lines

- Imports are sorted alphabetically and deduplicated.
- Multiple consecutive blank lines are collapsed to one.
- No leading blank line after `module` header.

## Line Wrapping

The layout engine uses `prettier` (`Text.PrettyPrint.Bernardy`) with the configured line length. Expressions and declarations are broken across lines when they exceed the limit.

## CLI Modes

| Flag | Behaviour |
|------|-----------|
| `--check` | Read file, format, and report if output differs from input. Exit code 0 if already formatted. |
| `--inplace` | Overwrite files with formatted output. |
| `--stdin` | Read source from stdin and write formatted output to stdout. |

---

*This style guide is enforced automatically by `idris2-fmt`. Do not manually adjust formatting — run the tool instead.*
