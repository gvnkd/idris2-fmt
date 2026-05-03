module IdrisFmt.Transform

import Data.List as L

import IdrisFmt.AST as AST
import IdrisFmt.Config as CFG

%default total

||| Apply all AST transformations before printing.
||| This is the fusion point: multiple passes composed into
||| a single function pipeline.
export
transformModule : CFG.Config -> List (AST.Decl AST.Name) -> List (AST.Decl AST.Name)
transformModule cfg = sortImports . mergeBlankLines
  where
    ||| Sort import declarations alphabetically.
    ||| Non-import declarations remain in their original positions.
    sortImports : List (AST.Decl AST.Name) -> List (AST.Decl AST.Name)
    sortImports decls = ?rhs_sortImports

    ||| Merge consecutive blank-line declarations.
    mergeBlankLines : List (AST.Decl AST.Name) -> List (AST.Decl AST.Name)
    mergeBlankLines decls = ?rhs_mergeBlankLines
