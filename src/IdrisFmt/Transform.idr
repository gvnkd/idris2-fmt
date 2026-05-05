module IdrisFmt.Transform
import Data.List as L
import IdrisFmt.AST as AST
import IdrisFmt.Config as CFG

%default total

isImport : AST.Decl AST.Name -> Bool
isImport (AST.DImport _) =
  True
isImport _ =
  False

importName : AST.Decl AST.Name -> List String
importName (AST.DImport imp) =
  AST.ImportDecl.name imp
importName _ =
  []

importCmp : AST.Decl AST.Name -> AST.Decl AST.Name -> Ordering
importCmp d1 d2 =
  compare (importName d1) (importName d2)

||| Collect a contiguous block of imports from the front of a list.
collectImports : List (AST.Decl AST.Name)
                   -> (List (AST.Decl AST.Name), List (AST.Decl AST.Name))
collectImports [] =
  ([], [])
collectImports (d@(AST.DImport _) :: rest) =
  let (imports, rest') = collectImports rest in (d :: imports, rest')
collectImports rest =
  ([], rest)

||| Sort import declarations alphabetically.
||| Non-import declarations remain in their original positions.
sortImports : List (AST.Decl AST.Name) -> List (AST.Decl AST.Name)
sortImports [] =
  []
sortImports (d :: rest) =
  if isImport d
    then let (imports, nonImports) = collectImports (d :: rest)
           in L.sortBy importCmp
                imports ++ sortImports (assert_smaller (d :: rest) nonImports)
    else d :: sortImports rest

||| Merge consecutive blank-line declarations into a single blank.
mergeBlankLines : List (AST.Decl AST.Name) -> List (AST.Decl AST.Name)
mergeBlankLines [] =
  []
mergeBlankLines (AST.DBlank _ :: rest) =
  case mergeBlankLines rest of
    (AST.DBlank _ :: rest') =>
      AST.DBlank 1 :: rest'
    rest' =>
      AST.DBlank 1 :: rest'
mergeBlankLines (d :: rest) =
  d :: mergeBlankLines rest

||| Apply all AST transformations before printing.
||| This is the fusion point: multiple passes composed into
||| a single function pipeline.
export transformModule : CFG.Config
                           -> List (AST.Decl AST.Name)
                                -> List (AST.Decl AST.Name)
transformModule cfg =
  sortImports . mergeBlankLines
