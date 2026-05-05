# Codebase Analysis Report — idris2-fmt

Generated: 2026-05-05

---

## P0 — Critical

### 1. `tyDoc` in `where` inside exported `Pretty` instance — known codegen crash

- **Location:** `src/IdrisFmt/Printer.idr`, lines 199–212
- **Current code:**
  ```idris
  export implementation Pretty (AST.DoStmt AST.Name) where
    prettyPrec _ (DoBind n rig ty tm) =
      prettyRig rig <+> pretty n <+> tyDoc ty <++> keyword "<-" <++> pretty tm
      where
        tyDoc : Maybe (AST.Expr AST.Name) -> Doc opts
        tyDoc Nothing = Doc.empty
        tyDoc (Just t) = space <+> colon <++> pretty t
    prettyPrec _ (DoBindPat pat ty val _) =
      pretty pat <+> tyDoc ty <++> keyword "<-" <++> pretty val
      where
        tyDoc : Maybe (AST.Expr AST.Name) -> Doc opts
        tyDoc Nothing = Doc.empty
        tyDoc (Just t) = space <+> colon <++> pretty t
  ```
- **Proposed fix:** Move `tyDoc` to a top-level function inside the `mutual` block (or before it), making it a sibling rather than a `where`-bound helper. Remove both `where` clauses.
  ```idris
  tyDoc : {opts : _} -> Maybe (AST.Expr AST.Name) -> Doc opts
  tyDoc Nothing = Doc.empty
  tyDoc (Just t) = space <+> colon <++> pretty t

  export implementation Pretty (AST.DoStmt AST.Name) where
    prettyPrec _ (DoBind n rig ty tm) =
      prettyRig rig <+> pretty n <+> tyDoc ty <++> keyword "<-" <++> pretty tm
    prettyPrec _ (DoBindPat pat ty val _) =
      pretty pat <+> tyDoc ty <++> keyword "<-" <++> pretty val
    ...
  ```
- **Feasibility:** **Easy.** No `parameters` blocks, no JSON constraints, no monad stack. The only dependency is that `tyDoc` must be visible to the `Pretty` instance; placing it at top level inside the existing `mutual` block satisfies this.

---

### 2. `ELocal` discards all local declarations — data loss

- **Location:** `src/IdrisFmt/Parser.idr`, line 213
- **Current code:**
  ```idris
  translatePTerm (PLocal _ decls scope) = AST.ELocal [] (translatePTerm scope)
  ```
- **Proposed fix:**
  ```idris
  translatePTerm (PLocal _ decls scope) =
    AST.ELocal (map (snd . translatePDecl) decls) (translatePTerm scope)
  ```
- **Feasibility:** **Easy.** Both `translatePTerm` and `translatePDecl` live in the same `mutual` block, so forward references are allowed. `translatePDecl` returns `(Nat, AST.Decl AST.Name)`; `snd` strips the line number. The `ELocal` constructor already accepts `List (Decl nm)`, so the types align.

---

### 3. Unimplemented stub `parseExpr`

- **Location:** `src/IdrisFmt/Parser.idr`, lines 536–537
- **Current code:**
  ```idris
  export parseExpr : String -> Either ParseError (AST.Expr AST.Name)
  parseExpr src = ?rhs_parseExpr
  ```
- **Proposed fix:** Either implement by calling `Parser.Source.runParser` with an expression parser (e.g., `Idris.Parser.expr`), or remove the export if it is dead code.
- **Feasibility:** **Easy to remove; medium to implement.** If the function is unused, deletion is trivial. If it must be kept, it follows the same pattern as `parseModule` (call `runParser` + `translatePTerm`). No blockers.

---

### 4. `translateConstant` catch-all silently swallows unknown primitives

- **Location:** `src/IdrisFmt/Parser.idr`, line 144
- **Current code:**
  ```idris
  translateConstant _ = AST.EPrim (AST.CInt 0)
  ```
- **Proposed fix:** Replace the catch-all with an explicit `EComment` placeholder (consistent with other unsupported features) or enumerate the remaining `Constant` constructors.
  ```idris
  translateConstant c =
    AST.EComment (MkComment LineComment ("constant: " ++ show c) 0 0) AST.EImplicit
  ```
- **Feasibility:** **Easy.** No type-system blockers. The `Show` instance for `Constant` is available from `Core.TT.Primitive`.

---

### 5. AST data constructors indented 16 spaces, violating 2-space rule

- **Location:** `src/IdrisFmt/AST.idr`, throughout (e.g., lines 10–12, 26–29, 33–36, etc.)
- **Current code:**
  ```idris
  public export data Name : Type where
                  UN : String -> Name
                  MN : String -> Int -> Name
                  NS : (List String) -> Name -> Name
  ```
- **Proposed fix:** Indent constructors by 2 spaces relative to `data`:
  ```idris
  public export data Name : Type where
    UN : String -> Name
    MN : String -> Int -> Name
    NS : List String -> Name -> Name
  ```
- **Feasibility:** **Trivial.** Pure formatting change; no semantic impact.

---

## P1 — High

### 6. `lines'` reimplements `Data.String.lines`

- **Location:** `src/IdrisFmt/Parser.idr`, lines 424–430
- **Current code:**
  ```idris
  lines' : String -> List String
  lines' s = go [] (unpack s)
    where
      go : List Char -> List Char -> List String
      go acc [] = [pack (reverse acc)]
      go acc ('\n' :: rest) = pack (reverse acc) :: go [] rest
      go acc (c :: rest) = go (c :: acc) rest
  ```
- **Proposed fix:** Delete `lines'` and use `Data.String.lines` (already imported as `S`). Verify behaviour on empty string (both return `[""]`).
  ```idris
  -- remove lines' entirely; use S.lines at call sites
  ```
- **Feasibility:** **Easy.** `Data.String` is imported as `S`. The only call site is `extractText` (line 435). Replace `lines' src` with `S.lines src`.

---

### 7. `spaces` reimplements `pack (replicate n ' ')`

- **Location:** `src/IdrisFmt/Align.idr`, lines 18–21
- **Current code:**
  ```idris
  spaces : Nat -> String
  spaces Z = ""
  spaces (S n) = " " ++ spaces n
  ```
- **Proposed fix:**
  ```idris
  spaces : Nat -> String
  spaces n = pack (replicate n ' ')
  ```
- **Feasibility:** **Easy.** `Data.String` is imported as `S` in `Align.idr`; `pack` and `replicate` are available from `Prelude`/`Data.List`.

---

### 8. `first` is a partial/artificial helper

- **Location:** `src/IdrisFmt/Align.idr`, lines 77–79
- **Current code:**
  ```idris
  first : List String -> String
  first [] = ""
  first (x :: _) = x
  ```
- **Proposed fix:** Eliminate `first` and pattern-match at the single call site (`mergeBlocks`), where the list is guaranteed non-empty because it came from `span`.
  ```idris
  mergeBlocks (x :: xs) (b :: bs) =
    case b of
      (y :: _) =>
        if x == y
          then b ++ mergeBlocks (drop (length b) (x :: xs)) bs
          else x :: mergeBlocks xs (b :: bs)
      [] => x :: mergeBlocks xs (b :: bs)
  ```
  Or simply use `head` with a default:
  ```idris
  mergeBlocks (x :: xs) (b :: bs) =
    if x == head "" b
      then ...
      else ...
  ```
- **Feasibility:** **Easy.** No blockers.

---

### 9. Massive `let` nesting in `translatePDecl`

- **Location:** `src/IdrisFmt/Parser.idr`, lines 337–341
- **Current code:**
  ```idris
  translatePDecl pdecl =
    let line = fcLine pdecl.fc
    in let d = val pdecl
       in pair line $ case d of
                        ...
  ```
- **Proposed fix:** Flatten to a single `let` block (Idris2 allows multiple bindings):
  ```idris
  translatePDecl pdecl =
    let line = fcLine pdecl.fc
        d    = val pdecl
     in pair line $ case d of
                      ...
  ```
- **Feasibility:** **Easy.** Same for the `NewPi` case (lines 214–221) and several others.

---

### 10. `implDeclDoc` has 4 nearly identical clauses

- **Location:** `src/IdrisFmt/Printer.idr`, lines 298–308
- **Current code:**
  ```idris
  implDeclDoc Nothing interfaceName params Nothing = ...
  implDeclDoc (Just n) interfaceName params Nothing = ...
  implDeclDoc Nothing interfaceName params (Just ds) = ...
  implDeclDoc (Just n) interfaceName params (Just ds) = ...
  ```
- **Proposed fix:** Factor into `headerDoc` and `bodyDoc`:
  ```idris
  implDeclDoc name iface params body =
    let header = keyword "implementation" <++>
                 maybe Doc.empty (\n => pretty n <++> equals) name <++>
                 pretty iface <++> hsep (map pretty params)
        bodyDoc = maybe Doc.empty (\ds => keyword "where" `vappend`
                                          indent 2 (vsep (map pretty ds))) body
     in case body of
          Nothing => header
          Just _  => header <++> bodyDoc
  ```
- **Feasibility:** **Easy.** No `parameters` blocks or auto-implicits prevent extraction. The function is already top-level.

---

### 11. `translatePClause` and `translatePClauseAsCase_` share duplicated `MkWithClause` / `MkImpossible` logic

- **Location:** `src/IdrisFmt/Parser.idr`, lines 59–65 and 329–334
- **Current code:** Both functions pattern-match on `MkPatClause`, `MkWithClause`, and `MkImpossible` with nearly identical RHSs for the latter two.
- **Proposed fix:** Introduce a helper that translates the common skeleton, parameterized by how to build the RHS:
  ```idris
  translatePClauseSkeleton : (IS.PTerm -> AST.Expr AST.Name) ->
                             (AST.Expr AST.Name -> AST.Expr AST.Name -> List (AST.Decl AST.Name) -> AST.Clause AST.Name) ->
                             IS.PClause -> AST.Clause AST.Name
  ```
  However, the payoff is small (only two call sites) and the functions differ in the `MkPatClause` case. **Document as low ROI** rather than refactor.
- **Feasibility:** **Achievable but low value.** Not blocked; just not worth the churn.

---

### 12. `mergeByLine` reimplements list merge

- **Location:** `src/IdrisFmt/Parser.idr`, lines 490–496
- **Current code:**
  ```idris
  mergeByLine [] ys = ys
  mergeByLine xs [] = xs
  mergeByLine ((lx, x) :: xs) ((ly, y) :: ys) =
    if lx <= ly
      then (lx, x) :: mergeByLine xs ((ly, y) :: ys)
      else (ly, y) :: mergeByLine ((lx, x) :: xs) ys
  ```
- **Proposed fix:** Use `Data.List.mergeBy` (already imported as `L`):
  ```idris
  mergeByLine = L.mergeBy (comparing fst)
  ```
- **Feasibility:** **Easy.** `Data.List` provides `mergeBy`. `comparing` is available from `Prelude` or `Data.Ord`.

---

### 13. Long lines exceeding 80 characters throughout `Parser.idr` and `Printer.idr`

- **Locations:**
  - `Parser.idr:21` — `translatePClauseAsCase_` signature (86 chars)
  - `Parser.idr:67` — `translatePFieldUpdate_` signature (104 chars)
  - `Parser.idr:74` — `translatePDo_` signature (95 chars)
  - `Parser.idr:148` — `PPi` translation (115 chars)
  - `Printer.idr:68` — `EPi Explicit` printing (138 chars)
  - `Printer.idr:76` — `EForall` printing (107 chars)
  - `Printer.idr:78` — `ELam` with `do` (130 chars)
  - `Printer.idr:82` — `ELet` printing (116 chars)
  - `Printer.idr:84` — `EApp` with `do` (117 chars)
  - `Printer.idr:189` — `MkClause` with `where` (116 chars)
- **Proposed fix:** Break signatures and expressions across lines using the layout engine or explicit newlines:
  ```idris
  translatePClauseAsCase_ :
    (IS.PTerm -> AST.Expr AST.Name) -> IS.PClause -> AST.Clause AST.Name
  ```
  ```idris
  prettyPrec d (EPi rig Explicit (Just n) arg ret) =
    parenthesise (d > Open) $
      parens (prettyRig rig <+> pretty n <++> colon <++> pretty arg)
        <++> line "->" <++> pretty ret
  ```
- **Feasibility:** **Easy but tedious.** No blockers; purely mechanical.

---

### 14. `AST.idr` unused `FieldUpdate` type

- **Location:** `src/IdrisFmt/AST.idr`, lines 146–148
- **Current code:**
  ```idris
  public export data FieldUpdate : Type -> Type where
                  FSet : List String -> Expr nm -> FieldUpdate nm
                  FSetApp : List String -> Expr nm -> FieldUpdate nm
  ```
- **Proposed fix:** Add an `EUpdate : Expr nm -> List (FieldUpdate nm) -> Expr nm` constructor to `Expr`, and handle it in `Parser.idr` (line 201, currently maps `PUpdate` to `EList`) and `Printer.idr`. Alternatively, remove `FieldUpdate` if record updates are intentionally out of scope.
- **Feasibility:** **Medium if adding `EUpdate`; trivial if deleting.** No JSON or `parameters` block constraints. Requires touching AST, Parser, and Printer. **Blocked only by scope decision** — is record update formatting in scope for this phase?

---

## P2 — Nice to Have

### 15. Unused `import Data.List as L` in `AST.idr`

- **Location:** `src/IdrisFmt/AST.idr`, line 2
- **Current code:** `import Data.List as L`
- **Issue:** `L` qualifier is never used in this module (`intersperse` is used unqualified, which works because `Prelude` re-exports it).
- **Proposed fix:** Remove the import or use `L.intersperse` for consistency.
- **Feasibility:** Trivial.

---

### 16. `pair` is a trivial eta-reducible wrapper

- **Location:** `src/IdrisFmt/Parser.idr`, lines 310–311
- **Current code:**
  ```idris
  pair : Nat -> AST.Decl AST.Name -> (Nat, AST.Decl AST.Name)
  pair line decl = (line, decl)
  ```
- **Proposed fix:** Inline `pair line $ ...` as `(line, ...)` at its single call site, or replace with `MkPair`.
- **Feasibility:** Trivial.

---

### 17. `isComment` and `isClaimDefPair` are single-use predicates

- **Location:** `src/IdrisFmt/Parser.idr`, lines 499–506
- **Issue:** These could be inlined into `insertBlanks` or kept as locals. Not a real problem.
- **Feasibility:** Trivial; low value.

---

### 18. `findCol` manually unpacks `String` instead of using library search

- **Location:** `src/IdrisFmt/Align.idr`, lines 9–16
- **Current code:**
  ```idris
  findCol needle haystack = go 1 (unpack haystack)
    where
      go : Nat -> List Char -> Maybe Nat
      go _ [] = Nothing
      go n cs@(_ :: rest) =
        if isPrefixOf (unpack needle) cs then Just n else go (S n) rest
  ```
- **Proposed fix:** If `Data.String` has a suitable `findSubseq` or `strIndex`, use it. Idris2's `base` does not currently expose a high-level substring-index function, so this manual loop may be the most portable approach. **Document as blocked by standard library gap.**
- **Feasibility:** **Blocked by lack of standard-library substring search.** The current implementation is correct and self-contained.

---

### 19. `processFile` and `run` are deeply nested

- **Location:** `src/Main.idr`, lines 26–56 and 60–74
- **Issue:** Multiple levels of `case` and `if` inside `do`. Could be flattened with helper functions (`readSrc`, `writeOut`, `checkDiff`) or by using `when`/`unless`.
- **Proposed fix:**
  ```idris
  processFile cfg check inplace file = do
    Right src <- SFRW.readFile file
      | Left err => printErr ("Error reading " ++ file) err $> False
    Right output <- pure (formatSource cfg src)
      | Left err => printErr ("Error formatting " ++ file) err $> False
    if check
       then pure (src /= output) <* when (src /= output) (putStrLn (file ++ " needs formatting"))
       else if inplace
              then SFRW.writeFile file output $> False
              else putStr output $> False
  ```
  (Using `Either` bind patterns if available; otherwise stick to `case` but extract helpers.)
- **Feasibility:** **Easy** but stylistic. No semantic change.

---

### 20. `translateDirective` is a large lookup table without grouping

- **Location:** `src/IdrisFmt/Parser.idr`, lines 233–269
- **Issue:** 37 clauses, many of which are simple string concatenations. Could be shortened with a helper that takes a prefix and a `Show`able value, but the win is marginal.
- **Feasibility:** Low value; leave as is.

---

## Summary Table

| # | Severity | File | Line(s) | Issue | Effort |
|---|----------|------|---------|-------|--------|
| 1 | P0 | Printer.idr | 199–212 | `tyDoc` in `where` inside exported `Pretty` | 5 min |
| 2 | P0 | Parser.idr | 213 | `ELocal` discards declarations | 5 min |
| 3 | P0 | Parser.idr | 536–537 | Unimplemented `parseExpr` | 5–30 min |
| 4 | P0 | Parser.idr | 144 | `translateConstant` catch-all | 5 min |
| 5 | P0 | AST.idr | 9–204 | 16-space constructor indentation | 10 min |
| 6 | P1 | Parser.idr | 424–430 | `lines'` reimplements `Data.String.lines` | 5 min |
| 7 | P1 | Align.idr | 18–21 | `spaces` reimplements `replicate` | 2 min |
| 8 | P1 | Align.idr | 77–79 | Partial `first` helper | 5 min |
| 9 | P1 | Parser.idr | 337–341 | Deep `let` nesting | 10 min |
| 10 | P1 | Printer.idr | 298–308 | `implDeclDoc` 4-way duplication | 10 min |
| 11 | P1 | Parser.idr | 59–65, 329–334 | `translatePClause` duplication | Low ROI |
| 12 | P1 | Parser.idr | 490–496 | `mergeByLine` manual merge | 5 min |
| 13 | P1 | Parser.idr / Printer.idr | many | Lines > 80 chars | 30 min |
| 14 | P1 | AST.idr | 146–148 | Unused `FieldUpdate` / missing `EUpdate` | Scope decision |
| 15 | P2 | AST.idr | 2 | Unused `import Data.List as L` | 1 min |
| 16 | P2 | Parser.idr | 310–311 | Trivial `pair` wrapper | 2 min |
| 17 | P2 | Parser.idr | 499–506 | Single-use predicates | 2 min |
| 18 | P2 | Align.idr | 9–16 | `findCol` manual char loop | Blocked (no stdlib fn) |
| 19 | P2 | Main.idr | 26–74 | Deep nesting in `processFile`/`run` | 15 min |
| 20 | P2 | Parser.idr | 233–269 | Verbose `translateDirective` | Low value |
