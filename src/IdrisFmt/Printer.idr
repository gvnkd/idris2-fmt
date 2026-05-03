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

%default covering

mutual
  prettyRig : {opts : _} -> AST.RigCount -> Doc opts
  prettyRig AST.Rig0 = line "0 "
  prettyRig AST.Rig1 = line "1 "
  prettyRig AST.RigW = Doc.empty

  lamBinder : {opts : _} -> AST.RigCount -> AST.Expr AST.Name -> AST.Expr AST.Name -> Doc opts
  lamBinder r p AST.EImplicit = prettyRig r <+> pretty p
  lamBinder r p t = prettyRig r <+> pretty p <++> colon <++> pretty t

  letBinder : {opts : _} -> AST.RigCount -> AST.Expr AST.Name -> AST.Expr AST.Name -> Doc opts
  letBinder r p AST.EImplicit = prettyRig r <+> pretty p
  letBinder r p t = prettyRig r <+> pretty p <++> colon <++> pretty t

  implNameDoc : {opts : _} -> Maybe AST.Name -> Doc opts
  implNameDoc Nothing  = Doc.empty
  implNameDoc (Just n) = pretty n <++> equals

  fnOptDoc : {opts : _} -> AST.FnOpt -> Doc opts
  fnOptDoc AST.Inline    = keyword "%inline"
  fnOptDoc AST.TCInline  = keyword "%tcinline"
  fnOptDoc AST.NoInline  = keyword "%noinline"

  paramDoc : {opts : _} -> (AST.Name, Maybe (AST.Expr AST.Name)) -> Doc opts
  paramDoc (n, Nothing) = pretty n
  paramDoc (n, Just ty) = pretty n <++> colon <++> pretty ty

  usingDoc : {opts : _} -> (Maybe AST.Name, AST.Expr AST.Name) -> Doc opts
  usingDoc (Nothing, ty) = pretty ty
  usingDoc (Just n, ty) = pretty n <++> colon <++> pretty ty

  fixityStr : AST.Fixity -> String
  fixityStr AST.InfixL = "infixl"
  fixityStr AST.InfixR = "infixr"
  fixityStr AST.Infix  = "infix"
  fixityStr AST.Prefix = "prefix"

  export
  Pretty AST.Name where
    prettyPrec _ (AST.UN s)   = D.ident s
    prettyPrec _ (AST.MN s i) = D.ident (s ++ "_" ++ show i)

  export
  Pretty (AST.Expr AST.Name) where
    prettyPrec _ (ERef n) = pretty n
    prettyPrec d (EPi rig Explicit (Just n) arg ret) =
      parenthesise (d > Open) $
        parens (prettyRig rig <+> pretty n <++> colon <++> pretty arg)
        <++> line "->" <++> pretty ret
    prettyPrec d (EPi rig Implicit (Just n) arg ret) =
      parenthesise (d > Open) $
        braces (prettyRig rig <+> pretty n <++> colon <++> pretty arg)
        <++> line "->" <++> pretty ret
    prettyPrec d (EPi rig Explicit Nothing arg ret) =
      parenthesise (d > Open) $
        pretty arg <++> line "->" <++> pretty ret
    prettyPrec d (EPi _ _ _ arg ret) =
      parenthesise (d > Open) $ pretty arg <++> line "->" <++> pretty ret
    prettyPrec d (ELam rig _ pat ty scope) =
      parenthesise (d > Open) $
        line "\\" <+> lamBinder rig pat ty <++> line "=>" <++> pretty scope
    prettyPrec d (ELet rig pat ty val scope _) =
      parenthesise (d > Open) $
        keyword "let" <++> letBinder rig pat ty <++> equals <++> pretty val
        `vappend` keyword "in" <++> pretty scope
    prettyPrec d (EApp f x) =
      parenthesise (d >= App) $ prettyPrec Open f <++> prettyPrec App x
    prettyPrec _ (ENamedApp f n x) =
      pretty f <++> braces (pretty n <++> equals <++> pretty x)
    prettyPrec _ (EAutoApp f x) =
      pretty f <++> pretty x
    prettyPrec _ (EDelayed x) = pretty x
    prettyPrec _ (EDelay x) = pretty x
    prettyPrec _ (EForce x) = pretty x
    prettyPrec _ (ECase scrut alts) =
      keyword "case" <++> pretty scrut <++> keyword "of"
      `vappend` indent 2 (vsep (map pretty alts))
    prettyPrec _ (ELocal decls scope) =
      (keyword "let" <++> keyword "where" `vappend` indent 2 (vsep (map pretty decls)))
      `vappend` (keyword "in" <++> pretty scope)
    prettyPrec _ (EList xs) = list (map pretty xs)
    prettyPrec _ (ESnocList xs) = snocList (map pretty (xs <>> []))
    prettyPrec _ (EPair x y) = tuple [pretty x, pretty y]
    prettyPrec _ (EString parts) = dquotes (hcat (map pretty parts))
    prettyPrec _ (EDo _ stmts) =
      keyword "do" `vappend` indent 2 (vsep (map pretty stmts))
    prettyPrec _ (EIdiom _ x) =
      lbracket <+> pipe <+> pretty x <+> pipe <+> rbracket
    prettyPrec _ (EIf c t f) =
      keyword "if" <++> pretty c
      <++> keyword "then" <++> pretty t
      <++> keyword "else" <++> pretty f
    prettyPrec _ (EHole s) = line "?" <+> line s
    prettyPrec _ EType = keyword "Type"
    prettyPrec _ EImplicit = line "_"
    prettyPrec _ (EQuote x) = line "`" <+> pretty x <+> line "`"
    prettyPrec _ (EUnquote x) = line "~" <+> pretty x
    prettyPrec _ (EPrim c) = pretty c
    prettyPrec d (EOp l op r) =
      parenthesise (d >= App) $ prettyPrec Open l <++> pretty op <++> prettyPrec Open r
    prettyPrec _ (EPrefixOp op x) = pretty op <++> pretty x
    prettyPrec _ (ESectionL op x) = parens (pretty op <++> pretty x)
    prettyPrec _ (ESectionR x op) = parens (pretty x <++> pretty op)
    prettyPrec _ (EBracketed x) = parens (pretty x)
    prettyPrec _ (EAs n x) = pretty n <+> line "@" <+> pretty x
    prettyPrec _ (EDotted x) = line "." <+> pretty x
    prettyPrec _ (EComment c x) = pretty c `vappend` pretty x

  export
  Pretty (AST.Decl AST.Name) where
    prettyPrec _ (DModule name _) = keyword "module" <++> line name
    prettyPrec _ (DImport imp) = pretty imp
    prettyPrec _ (DClaim comments n ty fnOpts) =
      let fnOptsDoc = hsep (map fnOptDoc fnOpts)
          base = case fnOpts of
                   [] => pretty n <++> colon <++> pretty ty
                   _  => fnOptsDoc <++> pretty n <++> colon <++> pretty ty
       in case comments of
            [] => base
            _  => vsep (map pretty comments) `vappend` base
    prettyPrec _ (DDef comments n clauses) =
      let body = vsep (map pretty clauses)
       in case comments of
            [] => body
            _  => vsep (map pretty comments) `vappend` body
    prettyPrec _ (DData _ dd) = pretty dd
    prettyPrec _ (DRecord _ rd) = pretty rd
    prettyPrec _ (DInterface _ id) = pretty id
    prettyPrec _ (DImpl _ impl) = pretty impl
    prettyPrec _ (DFixity fd) = pretty fd
    prettyPrec _ (DNamespace ns decls) =
      keyword "namespace" <++> hsep (map line ns) <++> keyword "where"
      `vappend` indent 2 (vsep (map pretty decls))
    prettyPrec _ (DMutual decls) =
      keyword "mutual" <++> keyword "where"
      `vappend` indent 2 (vsep (map pretty decls))
    prettyPrec _ (DParams params decls) =
      keyword "parameters" <++> parens (hsep (map paramDoc params))
      <++> keyword "where"
      `vappend` indent 2 (vsep (map pretty decls))
    prettyPrec _ (DUsing usings decls) =
      keyword "using" <++> parens (hsep (map usingDoc usings))
      <++> keyword "where"
      `vappend` indent 2 (vsep (map pretty decls))
    prettyPrec _ (DComment c) = pretty c
    prettyPrec _ (DBlank n) = vsep (replicate n (line ""))

  export
  Pretty (AST.Clause AST.Name) where
    prettyPrec _ (MkClause lhs rhs) =
      pretty lhs <++> keyword "=" <++> pretty rhs
    prettyPrec _ (MkWith lhs wps cs) =
      pretty lhs <++> keyword "with" <++> parens (hsep (map pretty wps))
      `vappend` indent 2 (vsep (map pretty cs))
    prettyPrec _ (MkImposs lhs) =
      pretty lhs <++> keyword "impossible"

  export
  Pretty (AST.DoStmt AST.Name) where
    prettyPrec _ (DoExp tm) = pretty tm
    prettyPrec _ (DoBind n rig ty tm) =
      prettyRig rig <+> pretty n <++> tyDoc ty <++> keyword "<-" <++> pretty tm
      where
        tyDoc : Maybe (AST.Expr AST.Name) -> Doc opts
        tyDoc Nothing = Doc.empty
        tyDoc (Just t) = colon <++> pretty t
    prettyPrec _ (DoBindPat pat ty val _) =
      pretty pat <++> tyDoc ty <++> keyword "<-" <++> pretty val
      where
        tyDoc : Maybe (AST.Expr AST.Name) -> Doc opts
        tyDoc Nothing = Doc.empty
        tyDoc (Just t) = colon <++> pretty t
    prettyPrec _ (DoLet n rig tm) =
      keyword "let" <++> prettyRig rig <+> pretty n <++> equals <++> pretty tm
    prettyPrec _ (DoLetPat pat val _) =
      keyword "let" <++> pretty pat <++> equals <++> pretty val
    prettyPrec _ (DoRewrite rule) =
      keyword "rewrite" <++> pretty rule

  export
  Pretty (AST.StringPart AST.Name) where
    prettyPrec _ (StrLit s) = text s
    prettyPrec _ (StrInterp tm) =
      line "\\{" <+> pretty tm <+> line "}"

  export
  Pretty (AST.FieldUpdate AST.Name) where
    prettyPrec _ (FSet path v) =
      hsep (map line path) <++> equals <++> pretty v
    prettyPrec _ (FSetApp path v) =
      hsep (map line path) <++> keyword "$=" <++> pretty v

  export
  Pretty (AST.ConDecl AST.Name) where
    prettyPrec _ (MkConDecl n ty) = pretty n <++> colon <++> pretty ty

  export
  Pretty (AST.FieldDecl AST.Name) where
    prettyPrec _ (MkFieldDecl n ty) = pretty n <++> colon <++> pretty ty

  export
  Pretty (AST.DataDecl AST.Name) where
    prettyPrec _ (MkDataDecl n params ty cons) =
      let paramsDoc = hsep (map (\(p, t) => parens (pretty p <++> colon <++> pretty t)) params)
          base = pretty n <++> colon <++> pretty ty <++> keyword "where"
          header = if null params then keyword "data" <++> base
                   else keyword "data" <++> pretty n <++> paramsDoc <++> colon <++> pretty ty <++> keyword "where"
       in header `vappend` indent 2 (vsep (map pretty cons))

  export
  Pretty (AST.RecordDecl AST.Name) where
    prettyPrec _ (MkRecordDecl n params _ fields) =
      let paramsDoc = hsep (map (\(p, ty) => parens (pretty p <++> colon <++> pretty ty)) params)
          base = pretty n <++> keyword "where"
          header = if null params then keyword "record" <++> base
                   else keyword "record" <++> pretty n <++> paramsDoc <++> keyword "where"
       in header `vappend` indent 2 (vsep (map pretty fields))

  export
  Pretty (AST.InterfaceDecl AST.Name) where
    prettyPrec _ (MkInterfaceDecl n params _ methods) =
      let paramsDoc = hsep (map (\(p, ty) => parens (pretty p <++> colon <++> pretty ty)) params)
          base = pretty n <++> keyword "where"
          header = if null params then keyword "interface" <++> base
                   else keyword "interface" <++> pretty n <++> paramsDoc <++> keyword "where"
       in header `vappend` indent 2 (vsep (map pretty methods))

  export
  Pretty (AST.ImplDecl AST.Name) where
    prettyPrec _ (MkImplDecl name interfaceName params body) =
      implDeclDoc name interfaceName params body

  export
  Pretty AST.FixityDecl where
    prettyPrec _ (MkFixityDecl fix prec names) =
      keyword (fixityStr fix) <++> line (show prec) <++> hsep (map (line . show) names)

  importDoc : {opts : _} -> Bool -> List String -> Maybe String -> Doc opts
  importDoc reexport name Nothing =
    if reexport
    then keyword "public" <++> keyword "export" <++> keyword "import" <++> line (concat (intersperse "." name))
    else keyword "import" <++> line (concat (intersperse "." name))
  importDoc reexport name (Just a) =
    if reexport
    then keyword "public" <++> keyword "export" <++> keyword "import" <++> line (concat (intersperse "." name)) <++> keyword "as" <++> line a
    else keyword "import" <++> line (concat (intersperse "." name)) <++> keyword "as" <++> line a

  export
  Pretty AST.ImportDecl where
    prettyPrec _ (MkImportDecl reexport name alias _ _) = importDoc reexport name alias

  export
  Pretty AST.Constant where
    prettyPrec _ (AST.CInt i)    = line (show i)
    prettyPrec _ (AST.CString s) = dquotes (text s)
    prettyPrec _ (AST.CChar c)   = squotes (line (show c))
    prettyPrec _ (AST.CDouble d) = line (show d)

  export
  Pretty (AST.OpStr AST.Name) where
    prettyPrec _ (AST.OpSymbols s) = D.operator_ s
    prettyPrec _ (AST.Backticked n) =
      enclose (D.operator_ "`") (D.operator_ "`") (pretty n)

  export
  Pretty C.Comment where
    prettyPrec _ (C.MkComment C.LineComment content _ _) =
      line "--" <+> text content
    prettyPrec _ (C.MkComment C.BlockComment content _ _) =
      line "{-" <+> text content <+> line "-}"

  implDeclDoc : {opts : _} -> Maybe AST.Name -> AST.Name -> List (AST.Expr AST.Name)
             -> Maybe (List (AST.Decl AST.Name)) -> Doc opts
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

||| Print a full module: render all declarations with inter-declaration spacing.
export
printModule : CFG.Config -> List (AST.Decl AST.Name) -> String
printModule cfg decls =
  let opts = D.toLayoutOpts cfg
   in Doc.render opts (vsep (map pretty decls))
