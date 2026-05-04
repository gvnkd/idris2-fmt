# idris2-fmt Codebase Analysis Report

Generated: 2026-05-04

---

## 1. Architecture & Language Constraints

### 1.1 — No `parameters` blocks
**Severity:** N/A (informational)
The codebase does not use `parameters` blocks. All functions pass arguments explicitly. This means functions CAN be extracted as first-class values freely — no blocker for refactoring.

### 1.2 — `mutual` blocks in Parser.idr and Printer.idr
**Severity:** N/A (informational)
- **Parser.idr** has two `mutual` blocks (lines 87–239 and 336–418). Functions inside cannot see top-level functions defined after the block (per Idris2 limitation documented in MEMORIES.md §24). This is why `translatePClauseAsCase_`, `translatePFieldUpdate_`, `translatePDo_` are defined OUTSIDE the mutual block as higher-order functions taking `trans` as parameter.
- **Printer.idr** has one huge `mutual` block (lines 16–321) containing all `Pretty` instances and helper functions. All helpers (`prettyRig`, `lamBinder`, `letBinder`, `implNameDoc`, `fnOptDoc`, `paramDoc`, `usingDoc`, `fixityStr`, `isOperatorChar`, `isOperatorName`, `conNameDoc`, `branchDoc`, `implDeclDoc`, `importDoc`, `interfaceParamDoc`, `showCharLit`) are inside this block because they share the `{opts : _}` implicit from the `Pretty` interface.
- **Feasibility note:** Functions inside the Printer mutual block CANNOT be extracted to the top level without threading `{opts : _}` explicitly. This limits refactoring of duplicated code inside this block.

### 1.3 — `%default covering` where `%default total` works
**Location:** `src/IdrisFmt/Align.idr:6`
**Severity:** P1
**Current code:**
```idris
%default covering
```
**Proposed fix:**
```idris
%default total
```
**Feasibility:** BLOCKED — `groupBlocks` is NOT structurally total. Idris2 can't see that `rest` (computed via `span`) is smaller. Verified against the actual project source: `idris2` rejects with "groupBlocks is not total, possibly not terminating due to recursive path". Align must stay `%default covering`.

### 1.4 — GADT-based AST with `nm` parameter
**Severity:** N/A (informational)
`Decl`, `Expr`, `Clause`, etc. are parameterized by `nm : Type`. No JSON serialization needed (formatter is a pipe: parse → transform → print). The `nm` parameter allows different name representations without breaking the AST. No blocker.

### 1.5 — Module dependency graph
```
Comments.idr → (standalone)
Config.idr → (standalone)
AST.idr → Comments.idr
Doc.idr → Config.idr, prettier
Align.idr → Config.idr
Transform.idr → AST.idr, Config.idr
Parser.idr → AST.idr, Comments.idr, idris2 compiler API
Printer.idr → AST.idr, Align.idr, Comments.idr, Config.idr, Doc.idr, prettier
CLI.idr → Config.idr
Main.idr → all of the above
```
Clean DAG, no cycles. Parser and Printer are the largest modules (555 and 328 lines). Printer cannot be split easily because all `Pretty` instances are in one `mutual` block.

### 1.6 — Namespace qualifiers
`Data.List as L`, `Data.String as S`, `Core.Core as CC`, etc. are used consistently and improve readability when qualified names appear (e.g., `L.sortBy`, `S.trim`). No unnecessary `Prelude.` qualification found.

### 1.7 — Monadic binding style
All monadic code in `Main.idr` uses `do`-notation. No raw `>>=` found where `do` would be more readable. No issue.

---

## 2. STYLE.md Compliance

### 2.1 — Line length violations (80-char limit)
**Severity:** P1
**Scope:** Widespread across Parser.idr, Printer.idr, CLI.idr

Top offenders:

| File | Line | Length | Description |
|------|------|--------|-------------|
| CLI.idr | 47 | 334 | `showUsage` — one massive string concatenation |
| Parser.idr | 396 | 183 | `translatePDecl` PImplementation branch |
| Parser.idr | 393 | 180 | `translatePDecl` PImplementation branch |
| Parser.idr | 221 | 168 | `translatePTerm` PLet branch |
| Printer.idr | 83 | 147 | `ELet` printer |
| Parser.idr | 501 | 146 | `extractComments` |
| Printer.idr | 79 | 145 | `ELam` with do printer |
| Printer.idr | 181 | 140 | `DParams` printer |
| Parser.idr | 376 | 140 | `translatePDecl` PData branch |
| Parser.idr | 359 | 140 | `translatePDecl` PClaim branch |

30+ lines exceed 80 chars. Most are in Parser.idr's `translatePTerm` / `translatePDecl` and Printer.idr's `Pretty` instances.

**Feasibility:** Straightforward to fix — wrap long expressions across multiple lines. No language constraint blocks this. The only consideration is that wrapping inside `mutual` block pattern matches must respect Idris2's layout rules.

### 2.2 — Indentation: 2 spaces, no tabs
**Severity:** N/A (compliant)
All source uses 2-space indentation consistently.

### 2.3 — `:=` in let bindings
**Severity:** N/A (not applicable)
STYLE.md describes the formatter's OUTPUT rules, not the codebase's own syntax. The codebase correctly uses standard Idris2 `let ... = ... in ...`.

### 2.4 — `do` on new line
**Severity:** N/A (compliant)
The printer correctly places `do` on a new line with indented body. Main.idr's own `do` blocks follow standard Idris2 layout.

### 2.5 — Named arguments for multi-parameter functions
**Severity:** P2
**Locations:** Multiple call sites use positional arguments for record constructors.

**Current code (CLI.idr:33):**
```idris
go rest (MkConfig n cfg.lineLength cfg.alignRules) c i s fs
```
**Proposed fix:**
```idris
go rest ({ indentWidth := n, lineLength := cfg.lineLength, alignRules := cfg.alignRules } cfg) c i s fs
```
Or using record update syntax if available in this Idris2 version.

**Current code (Parser.idr:439):**
```idris
MkImportDecl imp.reexport path alias Nothing Nothing
```
**Feasibility:** LOW PRIORITY — Idris2 supports named arguments with `{ recordField := value }`, but the current code is readable. The record constructors have few enough fields that positional is unambiguous. Not worth the churn.

### 2.6 — Blank lines between top-level definitions
**Severity:** N/A (compliant)
Top-level definitions are separated by blank lines consistently.

---

## 3. Anti-Patterns in Functional / Idris2 Code

### 3.1 — Manual recursion where stdlib exists
**Severity:** P1

**3.1a — `splitString` reimplements `String.split`**
**Location:** `src/IdrisFmt/Parser.idr:425–431`
**Current code:**
```idris
splitString : Char -> String -> List String
splitString c s = go [] (unpack s)
  where
    go : List Char -> List Char -> List String
    go acc [] = [pack (reverse acc)]
    go acc (x :: xs) =
      if x == c then pack (reverse acc) :: go [] xs else go (x :: acc) xs
```
**Proposed fix:**
```idris
-- Delete splitString entirely; use Data.String.split
-- At call site (line 435):
translateImport imp = let path = split (== '/') (toPath imp.path) in ...
```
**Feasibility:** VERIFIED — `String.split` compiles and has equivalent semantics.

**3.1b — `lines'` reimplements `lines` with different empty-string behavior**
**Location:** `src/IdrisFmt/Parser.idr:442–448`
**Current code:**
```idris
lines' : String -> List String
lines' s = go [] (unpack s)
  where
    go acc [] = [pack (reverse acc)]
    go acc ('\n' :: rest) = pack (reverse acc) :: go [] rest
    go acc (c :: rest) = go (c :: acc) rest
```
**Behavioral difference:** `lines' ""` returns `[""]`, `lines ""` returns `[]`. This difference matters for `extractText` (line 453) which does `L.drop startLn ls`. If `src` is empty, `lines' ""` = `[""]` vs `lines ""` = `[]`.
**Feasibility:** BLOCKED — replacing `lines'` with `lines` would change behavior for empty string edge case. Either verify the edge case is unreachable, or keep `lines'` with a comment documenting the intentional difference.

**3.1c — `list1ToList` reimplements `forget`**
**Location:** `src/IdrisFmt/Parser.idr:104–105`
**Current code:**
```idris
list1ToList : List1 a -> List a
list1ToList (x ::: xs) = x :: xs
```
**Proposed fix:** Replace all uses with `forget` from `Data.List1`.
```idris
-- Delete list1ToList
-- At call sites (lines 115, etc.):
forget names
```
**Feasibility:** VERIFIED — `forget` compiles and is semantically identical.

### 3.2 — Nested case/if chains where monadic composition would work
**Severity:** P2
**Location:** `src/Main.idr:28–58`
**Current code:**
```idris
processFile cfg check inplace file = do
  srcResult <- SFRW.readFile file
  case srcResult of
    Left err => do
      putStrLn ("Error reading " ++ file ++ ": " ++ show err)
      pure False
    Right src =>
      case formatSource cfg src of
        Left err => do
          putStrLn ("Error formatting " ++ file ++ ": " ++ show err)
          pure False
        Right output =>
          if check
            then
              if src == output
                then pure False
                else do ...
            else
              if inplace
                then do ...
                else do ...
```
**Proposed fix:** Factor out early-return pattern:
```idris
processFile cfg check inplace file = do
  Right src <- SFRW.readFile file
    | Left err => putStrLn ("Error reading " ++ file ++ ": " ++ show err) $> False
  Right output <- pure (formatSource cfg src)
    | Left err => putStrLn ("Error formatting " ++ file ++ ": " ++ show err) $> False
  handleOutput cfg check inplace file src output
```
**Feasibility:** ACHIEVABLE — Idris2 supports `pattern <- action | alternative` syntax. `$`>` might need to be defined or imported.

### 3.3 — Monolithic modules
**Severity:** P2
Parser.idr (555 lines) could be split into:
- `IdrisFmt.Parser.Translate` — AST translation functions
- `IdrisFmt.Parser.Comments` — comment extraction
- `IdrisFmt.Parser.Module` — top-level `parseModule`

**Feasibility:** BLOCKED — the translation functions are in a `mutual` block that references `translatePTerm`, `translatePClause`, `translatePDecl` mutually. Splitting would require breaking the mutual block into separate modules with forward declarations, which Idris2 doesn't support across modules. The mutual block stays.

Printer.idr (328 lines) is similarly locked by its mutual block.

---

## 4. Duplicated Code

### 4.1 — `lamBinder` and `letBinder` are IDENTICAL
**Severity:** P1
**Location:** `src/IdrisFmt/Printer.idr:21–26`
**Current code:**
```idris
lamBinder : {opts : _} -> AST.RigCount -> AST.Expr AST.Name -> AST.Expr AST.Name -> Doc opts
lamBinder r p AST.EImplicit = prettyRig r <+> pretty p
lamBinder r p t             = prettyRig r <+> pretty p <++> colon <++> pretty t

letBinder : {opts : _} -> AST.RigCount -> AST.Expr AST.Name -> AST.Expr AST.Name -> Doc opts
letBinder r p AST.EImplicit = prettyRig r <+> pretty p
letBinder r p t             = prettyRig r <+> pretty p <++> colon <++> pretty t
```
**Proposed fix:**
```idris
binderDoc : {opts : _} -> AST.RigCount -> AST.Expr AST.Name -> AST.Expr AST.Name -> Doc opts
binderDoc r p AST.EImplicit = prettyRig r <+> pretty p
binderDoc r p t             = prettyRig r <+> pretty p <++> colon <++> pretty t
```
Then rename `lamBinder` → `binderDoc`, `letBinder` → `binderDoc` at call sites (lines 22–23, 79–81, 83).
**Feasibility:** ACHIEVABLE — both are inside the same `mutual` block. Just delete one and rename.

### 4.2 — Comment-attachment pattern repeated 5 times
**Severity:** P1
**Locations:** `src/IdrisFmt/Printer.idr:150–152`, `155–157`, `161–162`, `165–166`, `170–171`
**Current code (repeated for DClaim, DDef, DData, DRecord, DInterface):**
```idris
case comments of
  [] => base
  _  => vsep (map pretty comments) `vappend` base
```
**Proposed fix:** Extract helper inside the mutual block:
```idris
withComments : {opts : _} -> List C.Comment -> Doc opts -> Doc opts
withComments [] base = base
withComments cs base = vsep (map pretty cs) `vappend` base
```
**Feasibility:** ACHIEVABLE — the helper can live inside the same `mutual` block and shares the `{opts : _}` implicit.

### 4.3 — `translatePClauseAsCase_` and `translatePClauseAsCase` are near-identical
**Severity:** P1
**Locations:** `src/IdrisFmt/Parser.idr:58–63` and `148–153`
**Current code (`translatePClauseAsCase`):**
```idris
translatePClauseAsCase : IS.PClause -> AST.Clause AST.Name
translatePClauseAsCase (MkPatClause _ lhs rhs _) =
  AST.MkCaseClause (translatePTerm lhs) (translatePTerm rhs)
translatePClauseAsCase (MkWithClause _ lhs wps _ _) =
  AST.MkCaseClause (translatePTerm lhs) (AST.EComment (MkComment LineComment "with clause" 0 0) (AST.EImplicit))
translatePClauseAsCase (MkImpossible _ lhs) = AST.MkImposs (translatePTerm lhs)
```
This is literally `translatePClauseAsCase_ translatePTerm`.
**But:** `translatePClauseAsCase` is DEAD CODE (see §7.1). It has ZERO call sites. So just delete it rather than deduplicate.

### 4.4 — `importDoc` has 4 branches with repeated module name rendering
**Severity:** P2
**Location:** `src/IdrisFmt/Printer.idr:277–285`
**Current code:**
```idris
importDoc : {opts : _} -> Bool -> List String -> Maybe String -> Doc opts
importDoc reexport name Nothing =
  if reexport
    then keyword "import" <++> keyword "public" <++> line (concat (intersperse "." name))
    else keyword "import" <++> line (concat (intersperse "." name))
importDoc reexport name (Just a) =
  if reexport
    then keyword "import" <++> keyword "public" <++> line (concat (intersperse "." name)) <++> keyword "as" <++> line a
    else keyword "import" <++> line (concat (intersperse "." name)) <++> keyword "as" <++> line a
```
**Proposed fix:**
```idris
importDoc : {opts : _} -> Bool -> List String -> Maybe String -> Doc opts
importDoc reexport name alias =
  let pub = if reexport then keyword "public" <++> empty else empty
      modName = line (concat (intersperse "." name))
      asDoc = case alias of
                Nothing => empty
                Just a  => keyword "as" <++> line a
  in keyword "import" <++> pub <+> modName <+> asDoc
```
**Feasibility:** VERIFIED — compiles and produces identical formatted output. `<++>` on `empty` does NOT add unwanted spaces (tested: `"import" <++> (keyword "public" <++> empty) <+> line "Foo"` renders as `"import public Foo"`).

### 4.5 — `implDeclDoc` has 4 branches with repeated header construction
**Severity:** P2
**Location:** `src/IdrisFmt/Printer.idr:311–321`
**Current code:**
```idris
implDeclDoc : {opts : _} -> Maybe AST.Name -> AST.Name -> List (AST.Expr AST.Name) -> Maybe (List (AST.Decl AST.Name)) -> Doc opts
implDeclDoc Nothing interfaceName params Nothing =
  keyword "implementation" <++> pretty interfaceName <++> hsep (map pretty params)
implDeclDoc (Just n) interfaceName params Nothing =
  keyword "implementation" <++> pretty n <++> equals <++> pretty interfaceName <++> hsep (map pretty params)
implDeclDoc Nothing interfaceName params (Just ds) =
  let header = keyword "implementation" <++> pretty interfaceName <++> hsep (map pretty params)
  in header <++> keyword "where" `vappend` indent 2 (vsep (map pretty ds))
implDeclDoc (Just n) interfaceName params (Just ds) =
  let header = keyword "implementation" <++> pretty n <++> equals <++> pretty interfaceName <++> hsep (map pretty params)
  in header <++> keyword "where" `vappend` indent 2 (vsep (map pretty ds))
```
**Proposed fix:** Factor header construction:
```idris
implDeclDoc name interfaceName params body =
  let nameDoc = case name of
                  Nothing => empty
                  Just n  => pretty n <++> equals <++> empty
      header = keyword "implementation" <++> nameDoc <+> pretty interfaceName <++> hsep (map pretty params)
  in case body of
       Nothing => header
       Just ds => header <++> keyword "where" `vappend` indent 2 (vsep (map pretty ds))
```
**Feasibility:** BLOCKED by MEMORIES.md §16 — `case` expressions inside `let` bindings with `Doc opts` type introduce fresh implicit `opts` variables that don't unify with outer scope. Verified: `case name of Nothing => Doc.empty; Just n => ...` inside a `let` produces "Can't solve constraint between: Doc ?opts and ?_". The original 4-branch pattern (matching on constructor at the function definition level) is the correct workaround. Do NOT attempt to simplify this.

---

## 5. Unnecessary Verbosity

### 5.1 — `showUsage` as one massive concatenation
**Severity:** P1
**Location:** `src/IdrisFmt/CLI.idr:47` (334 chars on one line)
**Current code:**
```idris
showUsage =
  "idris2-fmt [options] <files...>\n\n" ++ "Options:\n" ++ "  --check       Check formatting without writing\n" ++ "  --inplace     Edit files in place\n" ++ ...
```
**Proposed fix:**
```idris
showUsage = unlines
  [ "idris2-fmt [options] <files...>"
  , ""
  , "Options:"
  , "  --check       Check formatting without writing"
  , "  --inplace     Edit files in place"
  , "  --stdin       Read from stdin"
  , "  --indent N    Indentation width (default: 2)"
  , "  --width N     Line length (default: 80)"
  , "  --help        Show this help"
  ]
```
**Feasibility:** ACHIEVABLE — `unlines` is in Prelude.

### 5.2 — `alignLine` has redundant second `findCol` call
**Severity:** P1
**Location:** `src/IdrisFmt/Align.idr:44`
**Current code:**
```idris
alignLine token line targetCol =
  case findCol token line of
    Nothing => line
    Just col =>
      if col >= targetCol
        then line
        else let pad = targetCol `minus` col
             in case findCol token line of    -- REDUNDANT
                  Nothing => line
                  Just c =>
                    let n = c `minus` 1
                    in let (before, after) = splitAt n line
                       in before ++ spaces pad ++ after
```
The inner `findCol` call always returns `Just col` (same input, same result as the outer call).
**Proposed fix:**
```idris
alignLine token line targetCol =
  case findCol token line of
    Nothing => line
    Just col =>
      if col >= targetCol
        then line
        else let pad = targetCol `minus` col
                 n   = col `minus` 1
                 (before, after) = strSplitAt n line
             in before ++ spaces pad ++ after
  where
    strSplitAt : Nat -> String -> (String, String)
    strSplitAt n s = let bs = take n (unpack s)
                     in let as = drop n (unpack s)
                        in (pack bs, pack as)
```
Note: the `where` helper must be renamed from `splitAt` to `strSplitAt` because `Data.List.splitAt` is in scope and causes ambiguity.
**Feasibility:** VERIFIED — builds and produces identical formatted output.

### 5.3 — Trivial wrapper `pair`
**Severity:** P2
**Location:** `src/IdrisFmt/Parser.idr:319–320`
**Current code:**
```idris
pair : Nat -> AST.Decl AST.Name -> (Nat, AST.Decl AST.Name)
pair line decl = (line, decl)
```
Used once at line 350. The `(,)` constructor does the same thing.
**Proposed fix:** Replace `pair line $ case d of ...` with `(line, case d of ...)`.
**Feasibility:** ACHIEVABLE but trivial.

### 5.4 — `splitAt` in Align.idr reimplements `String.splitAt`
**Severity:** P2
**Location:** `src/IdrisFmt/Align.idr:51–54`
**Current code:**
```idris
splitAt : Nat -> String -> (String, String)
splitAt n s = let bs = take n (unpack s)
              in let as = drop n (unpack s)
                 in (pack bs, pack as)
```
**Proposed fix:** Replace with `String.splitAt` from stdlib (verified available).
**Feasibility:** PARTIALLY ACHIEVABLE — `String.splitAt` does NOT exist in Idris2's `Data.String`. Verified via REPL: only `Data.List.splitAt` exists. The custom `splitAt` is necessary but should be renamed to `strSplitAt` to avoid collision with `Data.List.splitAt` (which is auto-imported).

### 5.5 — Deep `let ... in let ... in let ...` nesting in `applyAlignment`
**Severity:** P2
**Location:** `src/IdrisFmt/Align.idr:103–118`
**Current code:**
```idris
applyAlignment cfg src =
  let rules = cfg.alignRules
  in let step1 = if rules.alignCaseArrows
                  then alignToken cfg.indentWidth " => " src
                  else src
     in let step2 = if rules.alignTypeSigs
                      then alignToken cfg.indentWidth " : " step1
                      else step1
        in let step3 = if rules.alignFunctionDefs
                         then alignToken cfg.indentWidth " = " step2
                         else step2
           in let step4 = if rules.alignRecordFields
                            then alignToken cfg.indentWidth " : " step3
                            else step3
              in step4
```
**Proposed fix:** Use `where` helper with `$` application chain:
```idris
applyAlignment cfg src =
  let rules = cfg.alignRules
  in applySteps rules.alignCaseArrows " => "
  $ applySteps rules.alignTypeSigs " : "
  $ applySteps rules.alignFunctionDefs " = "
  $ applySteps rules.alignRecordFields " : " src
  where
    applySteps : Bool -> String -> String -> String
    applySteps True  tok s = alignToken cfg.indentWidth tok s
    applySteps False _   s = s
```
**Feasibility:** VERIFIED — compiles and produces identical output. Note: `let` cannot define multi-parameter functions inline in Idris2; `where` is required.

### 5.6 — `maximumNat` could use existing patterns
**Severity:** P2
**Location:** `src/IdrisFmt/Align.idr:23–25`
**Current code:**
```idris
maximumNat : List Nat -> Maybe Nat
maximumNat [] = Nothing
maximumNat (x :: xs) = Just (foldl max x xs)
```
This is essentially `maxElem` from `Data.List`. But Idris2's `maxElem` returns the element type, not `Maybe`. The `Maybe` wrapper provides safety for empty lists. The current implementation is fine — just note it's a common pattern.

### 5.6 — `showCharLit` is `export` but only used internally
**Severity:** P2
**Location:** `src/IdrisFmt/Printer.idr:289`
**Current code:**
```idris
export showCharLit : Char -> String
```
Used only at line 298 inside the same module. Should be private.
**Feasibility:** ACHIEVABLE — change `export` to nothing (private).

---

## 6. Advanced Patterns — WITH FEASIBILITY CHECKS

### 6.1 — GADTs for CLI argument parsing
**Severity:** N/A (not recommended)
CLI.idr uses string matching for `--check`, `--inplace`, etc. A GADT-based approach would type-safety the parsed args. However:
- The CLI has 6 flags. String matching is proportional to complexity.
- No JSON serialization needed.
- No `parameters` block constraint.
- **Verdict:** Overkill. Current approach is fine.

### 6.2 — Effect system (`Control.App`)
**Severity:** N/A (not recommended)
The formatter uses `IO` + `Either ParseError`. The only effect is file I/O. Adding an effect system would add complexity with no benefit:
- Only 3 IO operations: `readFile`, `writeFile`, `putStr`
- Error handling is simple `Either`
- No resource management, no concurrency
- **Verdict:** Not worth it.

### 6.3 — Record-of-operations pattern
**Severity:** N/A (not applicable)
No `parameters` blocks to extract from. Functions are already first-class values.

---

## 7. Dead Code

### 7.1 — `translatePClauseAsCase` (without underscore) — ZERO call sites
**Severity:** P0
**Location:** `src/IdrisFmt/Parser.idr:148–153`
**Current code:**
```idris
translatePClauseAsCase : IS.PClause -> AST.Clause AST.Name
translatePClauseAsCase (MkPatClause _ lhs rhs _) =
  AST.MkCaseClause (translatePTerm lhs) (translatePTerm rhs)
translatePClauseAsCase (MkWithClause _ lhs wps _ _) =
  AST.MkCaseClause (translatePTerm lhs) (AST.EComment (MkComment LineComment "with clause" 0 0) (AST.EImplicit))
translatePClauseAsCase (MkImpossible _ lhs) = AST.MkImposs (translatePTerm lhs)
```
This is literally `translatePClauseAsCase_ translatePTerm` (lines 58–63), which IS used. The non-underscore version is called nowhere.
**Proposed fix:** Delete lines 148–153.
**Feasibility:** ACHIEVABLE — pure deletion, no impact.

### 7.2 — `parseExpr` is an unimplemented hole
**Severity:** P2
**Location:** `src/IdrisFmt/Parser.idr:554–555`
**Current code:**
```idris
export parseExpr : String -> Either ParseError (AST.Expr AST.Name)
parseExpr src = ?rhs_parseExpr
```
Exported but never called from Main.idr or CLI.idr. It's a planned feature (expression-level formatting) but currently dead code. The hole will cause issues if anyone tries to use it.
**Feasibility:** Keep if planning to implement; remove `export` and add a comment if not.

### 7.3 — Unused import: `Data.SnocList as SL` in AST.idr
**Severity:** P1
**Location:** `src/IdrisFmt/AST.idr:3`
`SL` qualifier is never used. `SnocList` type is available from Prelude without import.
**Proposed fix:** Remove `import Data.SnocList as SL`.
**Feasibility:** VERIFIED — `SnocList` is in Prelude.

### 7.4 — Unused import: `Data.Maybe as M` in Printer.idr
**Severity:** P1
**Location:** `src/IdrisFmt/Printer.idr:3`
`M.` qualifier is never used. `Maybe` constructors come from Prelude.
**Proposed fix:** Change to `import Data.Maybe` (if any `Data.Maybe` functions are used) or remove entirely.
**Feasibility:** ACHIEVABLE — check if any `Data.Maybe`-specific functions (like `mapMaybe`, `catMaybes`) are used. If not, remove entirely.

### 7.5 — Unused import: `Data.Either as E` in Main.idr
**Severity:** P1
**Location:** `src/Main.idr:2`
`E.` qualifier is never used.
**Proposed fix:** Remove `import Data.Either as E`.

### 7.6 — Unused import: `Text.PrettyPrint.Bernardy as PP` in Main.idr
**Severity:** P1
**Location:** `src/Main.idr:16`
`PP.` qualifier is never used in Main.idr. All pretty-printing is delegated to Printer module.
**Proposed fix:** Remove `import Text.PrettyPrint.Bernardy as PP`.

### 7.7 — Unused namespace qualifiers in Printer.idr
**Severity:** P2
**Locations:** `src/IdrisFmt/Printer.idr:11–12`
```idris
import Text.PrettyPrint.Bernardy.Combinators as PPC
import Text.PrettyPrint.Bernardy.Interface as PPI
```
`PPC.` and `PPI.` are never used. The imports themselves may be needed (they bring functions into scope), but the `as` aliases are unused.
**Proposed fix:** Remove `as PPC` and `as PPI` from the import lines.
**Feasibility:** ACHIEVABLE — the modules are still imported, just without aliases.

---

## 8. Function Fusions & Nested Cases

### 8.1 — `map (line . show)` repeated in Printer.idr
**Severity:** P2
**Locations:** `src/IdrisFmt/Printer.idr:93`, `96`, `276`

Lines 93–94:
```idris
let fieldDocs = map (line . show) fields
in pretty rec <+> hcat (concatMap (\f => [line ".", f]) fieldDocs)
```

Lines 95–97:
```idris
let fieldDocs = map (line . show) fields
in hcat (concatMap (\f => [line ".", f]) fieldDocs)
```

These are very similar — the only difference is whether `pretty rec` is prepended. Could factor the common part:
```idris
postfixDoc : {opts : _} -> List AST.Name -> Doc opts
postfixDoc fields =
  let fieldDocs = map (line . show) fields
  in hcat (concatMap (\f => [line ".", f]) fieldDocs)
```
**Feasibility:** ACHIEVABLE — extract into the mutual block.

### 8.2 — `map (translateName . snd)` pattern
**Severity:** P2
**Location:** `src/IdrisFmt/Parser.idr:169`
Already uses proper composition. No issue.

### 8.3 — `map (translateName . val . snd)` could be `map (translateName . val) . map snd`
**Severity:** N/A — the current code is already fused as `map (translateName . val . snd)` or similar. No issue found.

---

## 9. Unnecessary Parentheses

### 9.1 — No significant issues found
Parentheses usage is generally appropriate. In GADT constructor declarations, parentheses are required by Idris2 syntax. In pattern matches like `(x :: xs)`, the parens are necessary.

The only minor case is in `Align.idr:14`:
```idris
go n cs@(_ :: rest) =
```
The `(_ :: rest)` parens are required for the `@` pattern. No issue.

---

## 10. Test Framework

### 10.1 — Current framework assessment
**Severity:** N/A (informational)
**Current state:**
- Shell script (`tests/runtests.sh`) — 96 lines
- Two golden files: `Reference.idr` (correctly formatted), `broken.idr` (same code, bad formatting)
- Tests: compile reference, format reference (identity), format broken (convergence to reference), compile outputs
- No unit tests for individual AST translation functions
- No property-based tests

### 10.2 — Is the current framework sufficient?
**Severity:** P2
For a formatter, golden tests are the RIGHT primary approach. The current framework is acceptable for the current project size. However:

**Gaps:**
1. **No round-trip verification** — parse → format → parse should yield equivalent AST
2. **No individual translation tests** — if `translatePTerm` breaks for a specific constructor, the golden test might not catch it (the broken.idr might not exercise that constructor)
3. **No negative tests** — what happens with invalid input?
4. **No `--check` mode test** — the CLI's check mode is untested

### 10.3 — `idris2-hedgehog` vs `Test.Golden`
**Severity:** N/A (recommendation)
- **Hedgehog** would be useful for ONE property: "formatting is idempotent" (`format (format x) == format x`). But setting up hedgehog for this codebase requires adding it as a dependency and writing generators for the AST, which is significant effort.
- **Test.Golden** (from `idris2` package) would provide structured golden test management with diff output.
- **Current shell script** works and is pragmatic.
- **Verdict:** Current approach is sufficient. If the project grows to 20+ golden files, consider migrating to `Test.Golden`. Hedgehog is overkill.

### 10.4 — Test coverage gaps
**Severity:** P2
The `Reference.idr` test file covers:
- Data declarations, records, interfaces, implementations
- Function definitions with pattern matching
- Lambda, do-blocks, if-then-else
- String interpolation, lists, tuples, operators
- Mutual blocks, fixity declarations

**NOT covered by tests:**
- `parameters` blocks
- `using` blocks
- `mutual` blocks (inside parameters)
- Namespace declarations
- `%directive` declarations
- `%builtin` declarations
- `%transform` declarations
- `%runElab` declarations
- Nested `where` clauses with multiple definitions
- Idiom brackets (`[| ... |]`)
- Case expressions (standalone, not in function body)
- Record update syntax (`{ field := value }`)
- Snoc lists (`[< ...]`)
- Comprehensions, ranges
- `rewrite` in do-blocks

---

## Summary of Actionable Items by Priority

### P0 (Critical)
| # | Item | Location |
|---|------|----------|
| 7.1 | Delete dead `translatePClauseAsCase` | Parser.idr:148–153 |

### P1 (High)
| # | Item | Location |
|---|------|----------|
| 1.3 | ~~Switch Align.idr to `%default total`~~ BLOCKED | Align.idr:6 |
| 2.1 | Fix 30+ lines exceeding 80 chars | Parser.idr, Printer.idr, CLI.idr |
| 3.1a | Replace `splitString` with `String.split` | Parser.idr:425–431 |
| 3.1c | Replace `list1ToList` with `forget` | Parser.idr:104–105 |
| 4.1 | Merge identical `lamBinder`/`letBinder` | Printer.idr:21–26 |
| 4.2 | Extract `withComments` helper | Printer.idr (5 locations) |
| 4.3 | Delete dead `translatePClauseAsCase` (= 7.1) | Parser.idr:148–153 |
| 5.1 | Rewrite `showUsage` with `unlines` | CLI.idr:47 |
| 5.2 | Remove redundant `findCol` in `alignLine` (rename splitAt→strSplitAt) | Align.idr:44 |
| 5.4 | Rename custom `splitAt` to `strSplitAt` (stdlib has no String.splitAt) | Align.idr:51–54 |
| 5.5 | Simplify `applyAlignment` with `where` helper + `$` chain | Align.idr:103–118 |
| 5.6 | Make `showCharLit` private | Printer.idr:289 |
| 7.2 | Mark `parseExpr` as unimplemented or remove export | Parser.idr:554 |
| 7.7 | Remove unused `as PPC`/`as PPI` qualifiers | Printer.idr:11–12 |
| 8.1 | Extract `postfixDoc` helper | Printer.idr:93–97 |
| 10.2–10.4 | Expand test coverage | tests/ |
