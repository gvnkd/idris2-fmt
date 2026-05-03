module IdrisFmt.Printer

import Data.List as L
import Data.Maybe as M
import Data.String as S
import Text.PrettyPrint.Bernardy as PP
import Text.PrettyPrint.Bernardy.Combinators as PPC
import Text.PrettyPrint.Bernardy.Interface as PPI

import IdrisFmt.AST as AST
import IdrisFmt.Config as CFG
import IdrisFmt.Comments as C
import IdrisFmt.Doc as D

%default total

||| Print a full module: render all declarations with inter-declaration spacing.
export
printModule : {opts : _} -> CFG.Config -> List (AST.Decl AST.Name) -> Doc opts
printModule cfg decls = ?rhs_printModule

mutual
  ||| Pretty-print a name.
  export
  Pretty AST.Name where
    prettyPrec _ (AST.UN s)   = D.ident s
    prettyPrec _ (AST.MN s i) = D.ident (s ++ "_" ++ show i)

  ||| Pretty-print an expression.
  export
  Pretty (AST.Expr AST.Name) where
    prettyPrec p expr = ?rhs_prettyExpr

  ||| Pretty-print a top-level declaration.
  export
  Pretty (AST.Decl AST.Name) where
    prettyPrec p decl = ?rhs_prettyDecl

  ||| Pretty-print a pattern-matching clause.
  export
  Pretty (AST.Clause AST.Name) where
    prettyPrec p clause = ?rhs_prettyClause

  ||| Pretty-print a do-statement.
  export
  Pretty (AST.DoStmt AST.Name) where
    prettyPrec p stmt = ?rhs_prettyDoStmt

  ||| Pretty-print a string interpolation part.
  export
  Pretty (AST.StringPart AST.Name) where
    prettyPrec p sp = ?rhs_prettyStringPart

  ||| Pretty-print a record field update.
  export
  Pretty (AST.FieldUpdate AST.Name) where
    prettyPrec p fu = ?rhs_prettyFieldUpdate

  ||| Pretty-print a constructor declaration.
  export
  Pretty (AST.ConDecl AST.Name) where
    prettyPrec p cd = ?rhs_prettyConDecl

  ||| Pretty-print a record field declaration.
  export
  Pretty (AST.FieldDecl AST.Name) where
    prettyPrec p fd = ?rhs_prettyFieldDecl

  ||| Pretty-print a data type declaration.
  export
  Pretty (AST.DataDecl AST.Name) where
    prettyPrec p dd = ?rhs_prettyDataDecl

  ||| Pretty-print a record declaration.
  export
  Pretty (AST.RecordDecl AST.Name) where
    prettyPrec p rd = ?rhs_prettyRecordDecl

  ||| Pretty-print an interface declaration.
  export
  Pretty (AST.InterfaceDecl AST.Name) where
    prettyPrec p id = ?rhs_prettyInterfaceDecl

  ||| Pretty-print an implementation declaration.
  export
  Pretty (AST.ImplDecl AST.Name) where
    prettyPrec p impl = ?rhs_prettyImplDecl

  ||| Pretty-print a fixity declaration.
  export
  Pretty AST.FixityDecl where
    prettyPrec p f = ?rhs_prettyFixityDecl

  ||| Pretty-print an import declaration.
  export
  Pretty AST.ImportDecl where
    prettyPrec p imp = ?rhs_prettyImportDecl

  ||| Pretty-print a primitive constant.
  export
  Pretty AST.Constant where
    prettyPrec _ (AST.CInt i)    = line (show i)
    prettyPrec _ (AST.CString s) = dquotes (text s)
    prettyPrec _ (AST.CChar c)   = squotes (line (show c))
    prettyPrec _ (AST.CDouble d) = line (show d)

  ||| Pretty-print an operator string.
  export
  Pretty (AST.OpStr AST.Name) where
    prettyPrec _ (AST.OpSymbols s) = D.operator_ s
    prettyPrec _ (AST.Backticked n) =
      enclose (D.operator_ "`") (D.operator_ "`") (pretty n)

  ||| Pretty-print a comment.
  export
  Pretty C.Comment where
    prettyPrec _ (C.MkComment C.LineComment content _ _) =
      line "--" <+> text content
    prettyPrec _ (C.MkComment C.BlockComment content _ _) =
      line "{-" <+> text content <+> line "-}"
