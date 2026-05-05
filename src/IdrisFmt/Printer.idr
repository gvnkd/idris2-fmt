module IdrisFmt.Printer
import Data.List as L
import Data.String as S
import IdrisFmt.AST as AST
import IdrisFmt.Align as Align
import IdrisFmt.Comments as C
import IdrisFmt.Config as CFG
import IdrisFmt.Doc as D
import Text.PrettyPrint.Bernardy.Combinators
import Text.PrettyPrint.Bernardy.Interface

%default covering
-- Pure helpers (no type-class calls)
prettyRig : {opts : _} -> AST.RigCount -> Doc opts
prettyRig AST.Rig0 = line "0 "
prettyRig AST.Rig1 = line "1 "
prettyRig AST.RigW = Doc.empty

fixityStr : AST.Fixity -> String
fixityStr AST.InfixL = "infixl"
fixityStr AST.InfixR = "infixr"
fixityStr AST.Infix = "infix"
fixityStr AST.Prefix = "prefix"

isOperatorChar : Char -> Bool
isOperatorChar c =
  not (isAlpha c || isDigit c || c == '_' || c == '\'' || c == '"')

isOperatorName : AST.Name -> Bool
isOperatorName (AST.UN s) =
  case unpack s of
    [] => False
    (c :: cs) => isOperatorChar c && all isOperatorChar cs
isOperatorName _ = False

showCharLit : Char -> String
showCharLit '\n' = "\\n"
showCharLit '\t' = "\\t"
showCharLit '\\' = "\\\\"
showCharLit '\'' = "\\'"
showCharLit c = cast c
-- Forward declarations for instance bodies and helpers that call `pretty`
binderDoc : {opts : _}
              -> AST.RigCount
                   -> AST.Expr AST.Name -> AST.Expr AST.Name -> Doc opts

implNameDoc : {opts : _} -> Maybe AST.Name -> Doc opts

fnOptDoc : {opts : _} -> AST.FnOpt -> Doc opts

paramDoc : {opts : _} -> (AST.Name, Maybe (AST.Expr AST.Name)) -> Doc opts

usingDoc : {opts : _} -> (Maybe AST.Name, AST.Expr AST.Name) -> Doc opts

conNameDoc : {opts : _} -> AST.Name -> Doc opts

branchDoc : {opts : _} -> Doc opts -> AST.Expr AST.Name -> Doc opts

withComments : {opts : _} -> List C.Comment -> Doc opts -> Doc opts

visibilityDoc : {opts : _} -> AST.Visibility -> Doc opts

interfaceParamDoc : {opts : _} -> (AST.Name, AST.Expr AST.Name) -> Doc opts

importDoc : {opts : _} -> Bool -> List String -> Maybe String -> Doc opts

implDeclDoc : {opts : _}
                -> Maybe AST.Name
                     -> AST.Name
                          -> List (AST.Expr AST.Name)
                               -> Maybe (List (AST.Decl AST.Name)) -> Doc opts

prettyName : {opts : _} -> Prec -> AST.Name -> Doc opts

prettyExpr : {opts : _} -> Prec -> AST.Expr AST.Name -> Doc opts

prettyDecl : {opts : _} -> Prec -> AST.Decl AST.Name -> Doc opts

prettyClause : {opts : _} -> Prec -> AST.Clause AST.Name -> Doc opts

prettyDoStmt : {opts : _} -> Prec -> AST.DoStmt AST.Name -> Doc opts

prettyStringPart : {opts : _} -> Prec -> AST.StringPart AST.Name -> Doc opts

prettyConDecl : {opts : _} -> Prec -> AST.ConDecl AST.Name -> Doc opts

prettyFieldDecl : {opts : _} -> Prec -> AST.FieldDecl AST.Name -> Doc opts

prettyDataDecl : {opts : _} -> Prec -> AST.DataDecl AST.Name -> Doc opts

prettyRecordDecl : {opts : _} -> Prec -> AST.RecordDecl AST.Name -> Doc opts

prettyInterfaceDecl : {opts : _}
                        -> Prec -> AST.InterfaceDecl AST.Name -> Doc opts

prettyImplDecl : {opts : _} -> Prec -> AST.ImplDecl AST.Name -> Doc opts

prettyFixityDecl : {opts : _} -> Prec -> AST.FixityDecl -> Doc opts

prettyImportDecl : {opts : _} -> Prec -> AST.ImportDecl -> Doc opts

prettyConstant : {opts : _} -> Prec -> AST.Constant -> Doc opts

prettyOpStr : {opts : _} -> Prec -> AST.OpStr AST.Name -> Doc opts

prettyComment : {opts : _} -> Prec -> C.Comment -> Doc opts
-- Mutual block: minimal instance declarations only
mutual
  export
  implementation Pretty AST.Name where
    prettyPrec = prettyName
  export
  implementation Pretty (AST.Expr AST.Name) where
    prettyPrec = prettyExpr
  export
  implementation Pretty (AST.Decl AST.Name) where
    prettyPrec = prettyDecl
  export
  implementation Pretty (AST.Clause AST.Name) where
    prettyPrec = prettyClause
  export
  implementation Pretty (AST.DoStmt AST.Name) where
    prettyPrec = prettyDoStmt
  export
  implementation Pretty (AST.StringPart AST.Name) where
    prettyPrec = prettyStringPart
  export
  implementation Pretty (AST.ConDecl AST.Name) where
    prettyPrec = prettyConDecl
  export
  implementation Pretty (AST.FieldDecl AST.Name) where
    prettyPrec = prettyFieldDecl
  export
  implementation Pretty (AST.DataDecl AST.Name) where
    prettyPrec = prettyDataDecl
  export
  implementation Pretty (AST.RecordDecl AST.Name) where
    prettyPrec = prettyRecordDecl
  export
  implementation Pretty (AST.InterfaceDecl AST.Name) where
    prettyPrec = prettyInterfaceDecl
  export
  implementation Pretty (AST.ImplDecl AST.Name) where
    prettyPrec = prettyImplDecl
  export
  implementation Pretty AST.FixityDecl where
    prettyPrec = prettyFixityDecl
  export
  implementation Pretty AST.ImportDecl where
    prettyPrec = prettyImportDecl
  export
  implementation Pretty AST.Constant where
    prettyPrec = prettyConstant
  export
  implementation Pretty (AST.OpStr AST.Name) where
    prettyPrec = prettyOpStr
  export
  implementation Pretty C.Comment where
    prettyPrec = prettyComment
-- Helper definitions (after instances so `pretty` resolves)
binderDoc r p AST.EImplicit = prettyRig r <+> pretty p
binderDoc r p t = prettyRig r <+> pretty p <++> colon <++> pretty t
implNameDoc Nothing = Doc.empty
implNameDoc (Just n) = pretty n <++> equals
fnOptDoc AST.Inline = keyword "%inline"
fnOptDoc AST.TCInline = keyword "%tcinline"
fnOptDoc AST.NoInline = keyword "%noinline"
paramDoc (n, Nothing) = pretty n
paramDoc (n, Just ty) = pretty n <++> colon <++> pretty ty
usingDoc (Nothing, ty) = pretty ty
usingDoc (Just n, ty) = pretty n <++> colon <++> pretty ty
conNameDoc n = if isOperatorName n then parens (pretty n) else pretty n
branchDoc kw (EDo _ stmts) =
  hangSep' 2 (kw <++> keyword "do") (vsep (map pretty stmts))
branchDoc kw (EIf c t f) = kw `vappend` indent 2 (pretty (EIf c t f))
branchDoc kw expr = kw <++> pretty expr
withComments [] base = base
withComments cs base = vsep (map pretty cs) `vappend` base
visibilityDoc AST.Private = empty
visibilityDoc AST.Export = keyword "export"
visibilityDoc AST.Public = keyword "public" <++> keyword "export"
interfaceParamDoc (p, AST.EImplicit) = pretty p
interfaceParamDoc (p, ty) = parens (pretty p <++> colon <++> pretty ty)
importDoc reexport name alias =
  let pub = if reexport then [keyword "public"] else []
    in let modName = line (concat (intersperse "." name))
         in let asDoc = case alias of
                          Nothing => []
                          Just a => [keyword "as" <++> line a]
              in hsep ([keyword "import"] ++ pub ++ [modName] ++ asDoc)
implDeclDoc mn interfaceName params mbody =
  let nameDoc : List (Doc opts) = case mn of
                                    Nothing => []
                                    Just n => [pretty n, equals]
    in let header = hsep
                      (keyword
                         "implementation" :: nameDoc ++ [ pretty interfaceName
                                                        ] ++ map pretty params)
         in case mbody of
              Nothing => header
              Just _ => hangSep' 2 header (keyword "where")
prettyName _ (AST.UN s) = D.ident s
prettyName _ (AST.MN s i) = D.ident (s ++ "_" ++ show i)
prettyName _ (AST.NS ns n) =
  D.ident (concat (L.intersperse "." (reverse ns)) ++ ".") <+> pretty n
prettyExpr _ (ERef n) = pretty n
prettyExpr d (EPi rig Explicit (Just n) arg ret) =
  parenthesise
    (d > Open) $ hangSep' 2
                   (parens
                      (prettyRig rig <+> pretty n <++> colon <++> pretty arg))
                   (line "->" <++> pretty ret)
prettyExpr d (EPi rig Implicit (Just n) arg ret) =
  parenthesise
    (d > Open) $ hangSep' 2
                   (braces
                      (prettyRig rig <+> pretty n <++> colon <++> pretty arg))
                   (line "->" <++> pretty ret)
prettyExpr d (EPi rig Explicit Nothing arg ret) =
  parenthesise (d > Open) $ hangSep' 2 (pretty arg) (line "->" <++> pretty ret)
prettyExpr d (EPi _ _ _ arg ret) =
  parenthesise (d > Open) $ hangSep' 2 (pretty arg) (line "->" <++> pretty ret)
prettyExpr d (EForall ns scope) =
  parenthesise
    (d > Open) $ hangSep' 2
                   (keyword "forall" <++> hsep (map pretty ns) <++> line ".")
                   (pretty scope)
prettyExpr d (ELam rig _ pat ty (EDo _ stmts)) =
  parenthesise
    (d > Open) $ hangSep' 2
                   (line "\\" <+> binderDoc rig pat
                                    ty <++> line "=>" <++> keyword "do")
                   (vsep (map pretty stmts))
prettyExpr d (ELam rig _ pat ty scope) =
  parenthesise
    (d > Open) $ hangSep' 2 (line "\\" <+> binderDoc rig pat ty <++> line "=>")
                   (pretty scope)
prettyExpr d (ELet rig pat ty val scope _) =
  parenthesise
    (d > Open) $ hangSep' 2
                   (keyword "let" <++> binderDoc rig pat
                                         ty <++> equals <++> pretty val)
                   (keyword "in" <++> pretty scope)
prettyExpr d (EApp f (EDo _ stmts)) =
  parenthesise (d >= App) $ hangSep' 2 (prettyPrec Open f <++> keyword "do")
                              (vsep (map pretty stmts))
prettyExpr d (EApp f x) =
  parenthesise (d >= App) $ hangSep' 2 (prettyPrec Open f) (prettyPrec App x)
prettyExpr _ (ENamedApp f n x) =
  hangSep' 2 (pretty f) (braces (pretty n <++> equals <++> pretty x))
prettyExpr _ (EAutoApp f x) = hangSep' 2 (pretty f) (pretty x)
prettyExpr _ (EWithApp f x) =
  hangSep' 2 (pretty f) (keyword "with" <++> pretty x)
prettyExpr _ (EPostfixApp rec fields) =
  let fieldDocs = map (line . show) fields
    in pretty rec <+> hcat (concatMap (\f => [line ".", f]) fieldDocs)
prettyExpr _ (EPostfixAppPartial fields) =
  let fieldDocs = map (line . show) fields
    in hcat (concatMap (\f => [line ".", f]) fieldDocs)
prettyExpr _ (EDelayed x) = pretty x
prettyExpr _ (EDelay x) = pretty x
prettyExpr _ (EForce x) = pretty x
prettyExpr _ (ECase scrut alts) =
  keyword
    "case" <++> pretty scrut <++> keyword
                                    "of" `vappend` indent 2
                                                     (vsep (map pretty alts))
prettyExpr _ (ELocal decls scope) =
  let declsDoc = vsep (map pretty decls)
    in (keyword "let" `vappend` indent 2
                                  declsDoc) `vappend` (keyword
                                                         "in" <++> pretty scope)
prettyExpr _ (EList xs) = list (map pretty xs)
prettyExpr _ (ESnocList xs) = snocList (map pretty (xs <>> []))
prettyExpr _ (EPair x y) =
  lparen <+> pretty x <+> text ", " <+> pretty y <+> rparen
prettyExpr _ (EString parts) = dquotes (hcat (map pretty parts))
prettyExpr _ (EDo _ stmts) =
  keyword "do" `vappend` indent 2 (vsep (map pretty stmts))
prettyExpr _ (EIdiom _ x) = lbracket <+> pipe <+> pretty x <+> pipe <+> rbracket
prettyExpr _ (EIf c t f) =
  let cond = keyword "if" <++> pretty c
    in let thenBranch = branchDoc (keyword "then") t
         in let elseBranch = branchDoc (keyword "else") f
              in let horizontal = cond <++> thenBranch <++> elseBranch
                   in let thenDoc = cond `vappend` indent 2 thenBranch
                        in let vertical = thenDoc `vappend` indent 2 elseBranch
                             in ifMultiline horizontal vertical
prettyExpr _ (EHole s) = line "?" <+> line s
prettyExpr _ EType = keyword "Type"
prettyExpr _ EUnit = line "()"
prettyExpr _ EImplicit = line "_"
prettyExpr _ (EQuote x) = line "`" <+> pretty x <+> line "`"
prettyExpr _ (EUnquote x) = line "~" <+> pretty x
prettyExpr _ (EPrim c) = pretty c
prettyExpr d (EOp l op r) =
  parenthesise (d >= App) $ prettyPrec Open
                              l <++> pretty op <++> prettyPrec Open r
prettyExpr _ (EPrefixOp op x) = pretty op <++> pretty x
prettyExpr _ (ESectionL op x) = parens (pretty op <++> pretty x)
prettyExpr _ (ESectionR x op) = parens (pretty x <++> pretty op)
prettyExpr _ (EBracketed x) = parens (pretty x)
prettyExpr _ (EAs n x) = pretty n <+> line "@" <+> pretty x
prettyExpr _ (EDotted x) = line "." <+> pretty x
prettyExpr _ (EComment c x) = pretty c `vappend` pretty x
prettyDecl _ (DModule name _) = keyword "module" <++> line name
prettyDecl _ (DImport imp) = pretty imp
prettyDecl _ (DClaim comments vis n ty fnOpts) =
  let fnOptsDoc = hsep (map fnOptDoc fnOpts)
    in let base = case fnOpts of
                    [] => pretty n <++> colon <++> pretty ty
                    _ => fnOptsDoc <++> pretty n <++> colon <++> pretty ty
         in let full = case vis of
                         Private => base
                         _ => visibilityDoc vis <++> base
              in withComments comments full
prettyDecl _ (DDef comments n clauses) =
  let body = vsep (map pretty clauses) in withComments comments body
prettyDecl _ (DData comments vis dd@(MkDataDecl _ _ _ cons)) =
  let header = pretty dd
    in let body = case cons of
                    [] => empty
                    _ => indent 2 (vsep (map pretty cons))
         in let full = header `vappend` body
              in case vis of
                   Private => withComments comments full
                   _ => withComments comments (visibilityDoc vis `vappend` full)
prettyDecl _ (DRecord comments vis rd@(MkRecordDecl _ _ conName fields)) =
  let header = pretty rd
    in let conDoc = case conName of
                      Nothing => []
                      Just c => [keyword "constructor" <++> pretty c]
         in let allFields = conDoc ++ map pretty fields
              in let body = case allFields of
                              [] => empty
                              _ => indent 2 (vsep allFields)
                   in let full = header `vappend` body
                        in case vis of
                             Private => withComments comments full
                             _ =>
                               withComments comments
                                 (visibilityDoc vis `vappend` full)
prettyDecl _ (DInterface comments vis id@(MkInterfaceDecl _ _ _ methods)) =
  let header = pretty id
    in let body = case methods of
                    [] => empty
                    _ => indent 2 (vsep (map pretty methods))
         in let full = header `vappend` body
              in case vis of
                   Private => withComments comments full
                   _ => withComments comments (visibilityDoc vis `vappend` full)
prettyDecl _ (DImpl _ vis impl@(MkImplDecl _ _ _ body)) =
  let full = case body of
               Nothing => pretty impl
               Just ds => pretty impl `vappend` indent 2 (vsep (map pretty ds))
    in case vis of
         Private => full
         _ => visibilityDoc vis `vappend` full
prettyDecl _ (DFixity fd) = pretty fd
prettyDecl _ (DNamespace ns decls) =
  keyword
    "namespace" <++> hsep
                       (map line ns) <++> keyword
                                            "where" `vappend` indent 2
                                                                (vsep
                                                                   (map pretty
                                                                      decls))
prettyDecl _ (DMutual decls) =
  keyword "mutual" `vappend` indent 2 (vsep (map pretty decls))
prettyDecl _ (DParams params decls) =
  let header = keyword "parameters" <++> parens (hsep (map paramDoc params))
    in header `vappend` indent 2
                          (keyword
                             "where" `vappend` indent 2
                                                 (vsep (map pretty decls)))
prettyDecl _ (DUsing usings decls) =
  keyword
    "using" <++> parens
                   (hsep
                      (map usingDoc
                         usings)) <++> keyword
                                             "where" `vappend` indent 2
                                                                 (vsep
                                                                    (map
                                                                       pretty
                                                                       decls))
prettyDecl _ (DDirective s) = keyword "%" <+> line s
prettyDecl _ (DBuiltin bt n) = keyword "%builtin" <++> line bt <++> pretty n
prettyDecl _ (DTransform name lhs rhs) =
  hangSep' 2
    (keyword "%transform" <++> line name <++> pretty lhs <++> keyword "=")
    (pretty rhs)
prettyDecl _ (DRunElab tm) = keyword "%runElab" <++> pretty tm
prettyDecl _ (DComment c) = pretty c
prettyDecl _ (DBlank n) = vsep (replicate n (line ""))
prettyClause _ (MkClause lhs (EDo _ stmts) ws) =
  let body = hangSep' 2 (pretty lhs <++> keyword "=" <++> keyword "do")
               (vsep (map pretty stmts))
    in case ws of
         [] => body
         _ =>
           body `vappend` indent 2
                            (keyword "where" `vappend` indent 2
                                                         (vsep (map pretty ws)))
prettyClause _ (MkClause lhs rhs ws) =
  let body = hangSep 2 (pretty lhs <++> keyword "=") (pretty rhs)
    in case ws of
         [] => body
         _ =>
           body `vappend` indent 2
                            (keyword "where" `vappend` indent 2
                                                         (vsep (map pretty ws)))
prettyClause _ (MkCaseClause lhs (EDo _ stmts)) =
  hangSep' 2 (pretty lhs <++> keyword "=>" <++> keyword "do")
    (vsep (map pretty stmts))
prettyClause _ (MkCaseClause lhs rhs) =
  hangSep' 2 (pretty lhs <++> keyword "=>") (pretty rhs)
prettyClause _ (MkWith lhs wps cs) =
  pretty
    lhs <++> keyword
               "with" <++> parens
                             (hsep
                                (map pretty
                                   wps)) `vappend` indent 2
                                                          (vsep (map pretty cs))
prettyClause _ (MkImposs lhs) = pretty lhs <++> keyword "impossible"
prettyDoStmt _ (DoExp tm) = pretty tm
prettyDoStmt _ (DoBind n rig ty tm) =
  hangSep' 2 (prettyRig rig <+> pretty n <+> tyDoc ty <++> keyword "<-")
    (pretty tm)
  where
    tyDoc : Maybe (AST.Expr AST.Name) -> Doc opts
    tyDoc Nothing = Doc.empty
    tyDoc (Just t) = space <+> colon <++> pretty t
prettyDoStmt _ (DoBindPat pat ty val _) =
  hangSep' 2 (pretty pat <+> tyDoc ty <++> keyword "<-") (pretty val)
  where
    tyDoc : Maybe (AST.Expr AST.Name) -> Doc opts
    tyDoc Nothing = Doc.empty
    tyDoc (Just t) = space <+> colon <++> pretty t
prettyDoStmt _ (DoLet n rig tm) =
  hangSep' 2 (keyword "let" <++> prettyRig rig <+> pretty n <++> equals)
    (pretty tm)
prettyDoStmt _ (DoLetPat pat val _) =
  hangSep' 2 (keyword "let" <++> pretty pat <++> equals) (pretty val)
prettyDoStmt _ (DoRewrite rule) = keyword "rewrite" <++> pretty rule
prettyStringPart _ (StrLit s) = text s
prettyStringPart _ (StrInterp tm) = line "\\{" <+> pretty tm <+> line "}"
prettyConDecl _ (MkConDecl n ty) = conNameDoc n <++> colon <++> pretty ty
prettyFieldDecl _ (MkFieldDecl n ty) = pretty n <++> colon <++> pretty ty
prettyDataDecl _ (MkDataDecl n params ty cons) =
  let paramsDoc = hsep
                    (map
                       (\(p, t) => parens (pretty p <++> colon <++> pretty t))
                       params)
    in let base = pretty n <++> colon <++> pretty ty <++> keyword "where"
         in if null params
              then keyword "data" <++> base
              else hsep
                     [ keyword "data"
                     , pretty n
                     , paramsDoc
                     , colon
                     , pretty ty
                     , keyword "where"
                     ]
prettyRecordDecl _ (MkRecordDecl n params conName fields) =
  let paramsDoc = hsep
                    (map
                       (\(p, ty) =>
                          parens (pretty p <++> colon <++> pretty ty))
                       params)
    in if null params
         then keyword "record" <++> pretty n <++> keyword "where"
         else hsep [keyword "record", pretty n, paramsDoc, keyword "where"]
prettyInterfaceDecl _ (MkInterfaceDecl n params _ methods) =
  let paramsDoc = hsep (map interfaceParamDoc params)
    in if null params
         then keyword "interface" <++> pretty n <++> keyword "where"
         else hsep [keyword "interface", pretty n, paramsDoc, keyword "where"]
prettyImplDecl _ (MkImplDecl name interfaceName params body) =
  implDeclDoc name interfaceName params body
prettyFixityDecl _ (MkFixityDecl fix prec names) =
  keyword
    (fixityStr fix) <++> line (show prec) <++> hsep (map (line . show) names)
prettyImportDecl _ (MkImportDecl reexport name alias _ _) =
  importDoc reexport name alias
prettyConstant _ (AST.CInt i) = line (show i)
prettyConstant _ (AST.CString s) = dquotes (text s)
prettyConstant _ (AST.CChar c) = squotes (line (showCharLit c))
prettyConstant _ (AST.CDouble d) = line (show d)
prettyConstant _ AST.CWorldVal = text "WorldVal"
prettyOpStr _ (AST.OpSymbols s) = D.operator_ s
prettyOpStr _ (AST.Backticked n) =
  enclose (D.operator_ "`") (D.operator_ "`") (pretty n)
prettyComment _ (C.MkComment C.LineComment content _ _) =
  text "-- " <+> text content
prettyComment _ (C.MkComment C.BlockComment content _ _) =
  text ("{- " ++ content ++ " -}")
prettyComment _ (C.MkComment C.DocComment content _ _) =
  text "||| " <+> text content
-- Instance body definitions
||| Print a full module: render all declarations with inter-declaration spacing.
export printModule : CFG.Config -> List (AST.Decl AST.Name) -> String
printModule cfg decls =
  let opts = D.toLayoutOpts cfg
    in let rendered = Doc.render opts (vsep (map pretty decls))
         in Align.applyAlignment cfg rendered
