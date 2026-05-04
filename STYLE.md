# idris2-fmt Style Guide

This document describes the formatting rules enforced by `idris2-fmt` and how to configure them.

## Global Options

| Flag | Default | Description |
|------|---------|-------------|
| `--indent N` | `2` | Number of spaces per indentation level |
| `--width N` | `80` | Target line length for layout engine |

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
