module IdrisFmt.Printer
import Data.List as L
import Data.String as S
import IdrisFmt.AST as AST
import IdrisFmt.Align as Align
import IdrisFmt.Blocks as Blocks
import IdrisFmt.Comments as C
import IdrisFmt.Config as CFG
import IdrisFmt.Doc as D
import IdrisFmt.Measure as Measure
import Text.PrettyPrint.Bernardy.Combinators
import Text.PrettyPrint.Bernardy.Interface

%default covering
-- Pure helpers (no type-class calls)
prettyRig : {opts : _} -> AST.RigCount -> Doc opts
prettyRig AST.Rig0 =
  line "0 "
prettyRig AST.Rig1 =
  line "1 "
prettyRig AST.RigW =
  Doc.empty

fixityStr : AST.Fixity -> String
fixityStr AST.InfixL =
  "infixl"
fixityStr AST.InfixR =
  "infixr"
fixityStr AST.Infix =
  "infix"
fixityStr AST.Prefix =
  "prefix"

isOperatorChar : Char -> Bool
isOperatorChar c =
  not (isAlpha c || isDigit c || c == '_' || c == '\'' || c == '"')

isOperatorName : AST.Name -> Bool
isOperatorName (AST.UN s) =
  case unpack s of
    [] =>
      False
    (c :: cs) =>
      isOperatorChar c && all isOperatorChar cs
isOperatorName _ =
  False

showCharLit : Char -> String
showCharLit '\n' =
  "\\n"
showCharLit '\t' =
  "\\t"
showCharLit '\\' =
  "\\\\"
showCharLit '\'' =
  "\\'"
showCharLit c =
  cast c
-- Forward declarations for instance bodies and helpers that call `pretty`
binderDoc : {opts : _} -> AST.RigCount -> AST.Expr
                                            AST.Name -> AST.Expr
                                                          AST.Name -> Doc opts

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
              -> Maybe (List (AST.Decl AST.Name))
              -> Doc opts

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

prettyInterfaceDecl : {opts : _} -> Prec -> AST.InterfaceDecl
                                              AST.Name -> Doc opts

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
    prettyPrec =
      prettyName
  export
  implementation Pretty (AST.Expr AST.Name) where
    prettyPrec =
      prettyExpr
  export
  implementation Pretty (AST.Decl AST.Name) where
    prettyPrec =
      prettyDecl
  export
  implementation Pretty (AST.Clause AST.Name) where
    prettyPrec =
      prettyClause
  export
  implementation Pretty (AST.DoStmt AST.Name) where
    prettyPrec =
      prettyDoStmt
  export
  implementation Pretty (AST.StringPart AST.Name) where
    prettyPrec =
      prettyStringPart
  export
  implementation Pretty (AST.ConDecl AST.Name) where
    prettyPrec =
      prettyConDecl
  export
  implementation Pretty (AST.FieldDecl AST.Name) where
    prettyPrec =
      prettyFieldDecl
  export
  implementation Pretty (AST.DataDecl AST.Name) where
    prettyPrec =
      prettyDataDecl
  export
  implementation Pretty (AST.RecordDecl AST.Name) where
    prettyPrec =
      prettyRecordDecl
  export
  implementation Pretty (AST.InterfaceDecl AST.Name) where
    prettyPrec =
      prettyInterfaceDecl
  export
  implementation Pretty (AST.ImplDecl AST.Name) where
    prettyPrec =
      prettyImplDecl
  export
  implementation Pretty AST.FixityDecl where
    prettyPrec =
      prettyFixityDecl
  export
  implementation Pretty AST.ImportDecl where
    prettyPrec =
      prettyImportDecl
  export
  implementation Pretty AST.Constant where
    prettyPrec =
      prettyConstant
  export
  implementation Pretty (AST.OpStr AST.Name) where
    prettyPrec =
      prettyOpStr
  export
  implementation Pretty C.Comment where
    prettyPrec =
      prettyComment
-- Helper definitions (after instances so `pretty` resolves)
binderDoc r p AST.EImplicit =
  prettyRig r <+> pretty p
binderDoc r p t =
  prettyRig r <+> pretty p <++> colon <++> pretty t
implNameDoc Nothing =
  Doc.empty
implNameDoc (Just n) =
  pretty n <++> equals
fnOptDoc AST.Inline =
  keyword "%inline"
fnOptDoc AST.TCInline =
  keyword "%tcinline"
fnOptDoc AST.NoInline =
  keyword "%noinline"
paramDoc (n, Nothing) =
  pretty n
paramDoc (n, Just ty) =
  pretty n <++> colon <++> pretty ty
usingDoc (Nothing, ty) =
  pretty ty
usingDoc (Just n, ty) =
  pretty n <++> colon <++> pretty ty
conNameDoc n =
  pretty n
branchDoc kw (EDo _ stmts) =
  hangSep' 2 (kw <++> keyword "do") (vsep (map pretty stmts))
branchDoc kw (EIf c t f) =
  kw `vappend` indent 2 (pretty (EIf c t f))
branchDoc kw expr =
  kw <++> pretty expr
withComments [] base =
  base
withComments cs base =
  vsep (map pretty cs) `vappend` base
visibilityDoc AST.Private =
  empty
visibilityDoc AST.Export =
  keyword "export"
visibilityDoc AST.Public =
  keyword "public" <++> keyword "export"
interfaceParamDoc (p, AST.EImplicit) =
  pretty p
interfaceParamDoc (p, ty) =
  parens (pretty p <++> colon <++> pretty ty)
importDoc reexport name alias =
  let
    pub     = if reexport then [keyword "public"] else []
    modName = line (concat (intersperse "." name))
    asDoc   = case alias of
                Nothing =>
                  []
                Just a =>
                  [keyword "as" <++> line a]
  in hsep ([keyword "import"] ++ pub ++ [modName] ++ asDoc)
implDeclDoc mn interfaceName params mbody =
  let
    nameDoc : List (Doc opts) = case mn of
                                  Nothing =>
                                    []
                                  Just n =>
                                    [pretty n, equals]
    header                    = hsep
                                  (keyword "implementation"
                                   ::
                                     nameDoc
                                     ++
                                       [pretty interfaceName]
                                       ++
                                         map pretty params)
  in case mbody of
       Nothing =>
         header
       Just _ =>
         hangSep' 2 header (keyword "where")
prettyName _ (AST.UN s) =
  if isOperatorName (AST.UN s) then parens (D.ident s) else D.ident s
prettyName _ (AST.MN s i) =
  D.ident (s ++ "_" ++ show i)
prettyName _ (AST.NS ns n) =
  D.ident (concat (L.intersperse "." (reverse ns)) ++ ".") <+> pretty n

||| Check if an expression is "heavy" (if/case) and should start on its own line.
isHeavy : AST.Expr AST.Name -> Bool
isHeavy (EIf _ _ _) =
  True
isHeavy (ECase _ _) =
  True
isHeavy (EOp _ _ r) =
  isHeavy r
isHeavy _ =
  False
-- Block-aware pretty-printing helpers for let/pi alignment
prettyParamDoc : {opts : _}
                 -> AST.RigCount
                 -> AST.PiInfo (AST.Expr AST.Name)
                 -> Maybe AST.Name
                 -> AST.Expr AST.Name
                 -> Doc opts
prettyParamDoc rig Explicit (Just n) arg =
  parens (prettyRig rig <+> pretty n <++> colon <++> pretty arg)
prettyParamDoc rig Implicit (Just n) arg =
  braces (prettyRig rig <+> pretty n <++> colon <++> pretty arg)
prettyParamDoc rig AutoImplicit (Just n) arg =
  braces
    (keyword "auto" <++> prettyRig rig <+> pretty n <++> colon <++> pretty arg)
prettyParamDoc _ _ (Just n) arg =
  parens (pretty n <++> colon <++> pretty arg)
prettyParamDoc _ _ Nothing arg =
  pretty arg

piArrow : AST.PiInfo (AST.Expr AST.Name) -> Maybe AST.Name -> String
piArrow AutoImplicit Nothing = "=>"
piArrow _ _ = "->"

prettySinglePi : {opts : _}
                 -> Prec
                 -> List
                      (AST.RigCount, (AST.PiInfo
                                        (AST.Expr
                                           AST.Name), (Maybe
                                                         AST.Name, AST.Expr
                                                                     AST.Name)))
                 -> AST.Expr AST.Name
                 -> Doc opts
prettySinglePi d [(rig, (info, (n, arg)))] res =
  let arrow = piArrow info n
  in parenthesise (d > Open)
  $
    hangSep' 2 (prettyParamDoc rig info n arg) (line arrow <++> pretty res)
prettySinglePi _ _ res =
  pretty res

piParamDocs : {opts : _}
              -> List
                   (AST.RigCount, (AST.PiInfo
                                     (AST.Expr
                                        AST.Name), (Maybe AST.Name, AST.Expr
                                                                      AST.Name)))
              -> List (Doc opts)
piParamDocs [] =
  []
piParamDocs ((rig, (info, (n, arg))) :: rest) =
  prettyParamDoc rig info n arg :: piParamDocs rest

prettyPiBlock : {opts : _}
                -> Prec
                -> List
                     (AST.RigCount, (AST.PiInfo
                                       (AST.Expr
                                          AST.Name), (Maybe
                                                        AST.Name, AST.Expr
                                                                    AST.Name)))
                -> AST.Expr AST.Name
                -> Doc opts
prettyPiBlock d ps res =
  let
    paramItems : List (Doc opts, String)
      = map (\(rig, (info, (n, arg))) =>
               (prettyParamDoc rig info n arg, piArrow info n)) ps
    paramDocs = map fst paramItems
    arrows = map snd paramItems
    vert = case paramDocs of
              [] =>
                pretty res
              (p :: rest) =>
                let arrowDocs = map (\a => line (a ++ " ")) arrows
                in vsep (p :: zipWith (<+>) arrowDocs (rest ++ [pretty res]))
    horiz = hsep (concatMap (\(p, a) => [p, line a]) paramItems ++ [pretty res])
  in parenthesise (d > Open) $ horiz <|> vert

prettySingleLet : {opts : _}
                  -> Prec
                  -> AST.RigCount
                  -> AST.Expr AST.Name
                  -> AST.Expr AST.Name
                  -> AST.Expr AST.Name
                  -> AST.Expr AST.Name
                  -> Doc opts
prettySingleLet d rig pat ty val scope =
  parenthesise (d > Open)
  $
    hangSep' 2
      (keyword "let" <++> binderDoc rig pat ty <++> equals <++> pretty val)
      (keyword "in" <++> pretty scope)

letBindDocs : {opts : _}
              -> List
                   (AST.RigCount, (AST.Expr
                                     AST.Name, (AST.Expr
                                                  AST.Name, AST.Expr AST.Name)))
              -> List (Doc opts)
letBindDocs [] =
  []
letBindDocs ((rig, (pat, (ty, val))) :: rest) =
  (binderDoc rig pat ty <++> equals <++> pretty val) :: letBindDocs rest

letPatDocs : {opts : _}
             -> List
                  (AST.RigCount, (AST.Expr
                                    AST.Name, (AST.Expr
                                                 AST.Name, AST.Expr AST.Name)))
             -> List (Doc opts)
letPatDocs [] =
  []
letPatDocs ((rig, (pat, (ty, _))) :: rest) =
  (binderDoc rig pat ty) :: letPatDocs rest

letAlignedBinds : {opts : _}
                  -> Nat
                  -> List
                       (AST.RigCount, (AST.Expr
                                         AST.Name, (AST.Expr
                                                      AST.Name, AST.Expr
                                                                  AST.Name)))
                  -> List (Doc opts)
letAlignedBinds _ [] =
  []
letAlignedBinds w ((rig, (pat, (ty, val))) :: rest) =
  (Measure.padTo w (binderDoc rig pat ty) <++> equals <++> pretty val)
  ::
    letAlignedBinds w rest

prettyLetBlock : {opts : _}
                 -> Prec
                 -> List
                      (AST.RigCount, (AST.Expr
                                        AST.Name, (AST.Expr
                                                     AST.Name, AST.Expr
                                                                 AST.Name)))
                 -> AST.Expr AST.Name
                 -> Doc opts
prettyLetBlock d bs sc =
  let
    bindDocs : List (Doc opts)     = letBindDocs bs
    patDocs  : List (Doc opts)      = letPatDocs bs
    maxPatW                        = Measure.maxWidth
                                       (map Measure.measureWidth patDocs)
    alignedBinds : List (Doc opts) = letAlignedBinds maxPatW bs
    letKw                          = keyword "let"
    inKw                           = keyword "in"
    vert                           = (letKw
                                      `vappend`
                                        indent 2 (vsep alignedBinds))
                                     `vappend`
                                       (inKw <++> pretty sc)
  in parenthesise (d > Open) $ vert
prettyExpr _ (ERef n) =
  pretty n
prettyExpr d e@(EPi _ _ _ _ _) =
  case Blocks.flattenEPi e of
    Just (MkPiBlock ps res) =>
      if length ps == 1 then prettySinglePi d ps res else prettyPiBlock d ps res
    Nothing =>
      pretty e
prettyExpr d (EForall ns scope) =
  parenthesise (d > Open)
  $
    hangSep' 2 (keyword "forall" <++> hsep (map pretty ns) <++> line ".")
      (pretty scope)
prettyExpr d (ELam rig _ pat ty (EDo _ stmts)) =
  parenthesise (d > Open)
  $
    hangSep' 2
      (line "\\" <+> binderDoc rig pat ty <++> line "=>" <++> keyword "do")
      (vsep (map pretty stmts))
prettyExpr d (ELam rig _ pat ty scope) =
  parenthesise (d > Open)
  $
    hangSep' 2 (line "\\" <+> binderDoc rig pat ty <++> line "=>")
      (pretty scope)
prettyExpr d (ELet rig pat ty val scope _) =
  case Blocks.flattenELet (ELet rig pat ty val scope []) of
    Just (MkLetBlock bs sc, _) =>
      if length bs == 1
        then prettySingleLet d rig pat ty val sc
        else prettyLetBlock d bs sc
    Nothing =>
      prettySingleLet d rig pat ty val scope
prettyExpr d (EApp f (EDo _ stmts)) =
  parenthesise (d >= App)
  $
    hangSep' 2 (prettyPrec Open f <++> keyword "do") (vsep (map pretty stmts))
prettyExpr d (EApp f x) =
  parenthesise (d >= App) $ hangSep' 2 (prettyPrec Open f) (prettyPrec App x)
prettyExpr _ (ENamedApp f n x) =
  hangSep' 2 (pretty f) (braces (pretty n <++> equals <++> pretty x))
prettyExpr _ (EAutoApp f x) =
  hangSep' 2 (pretty f) (pretty x)
prettyExpr _ (EWithApp f x) =
  hangSep' 2 (pretty f) (keyword "with" <++> pretty x)
prettyExpr _ (EPostfixApp rec fields) =
  let fieldDocs = map (line . show) fields
    in pretty rec <+> hcat (concatMap (\f => [line ".", f]) fieldDocs)
prettyExpr _ (EPostfixAppPartial fields) =
  let fieldDocs = map (line . show) fields
    in hcat (concatMap (\f => [line ".", f]) fieldDocs)
prettyExpr _ (EDelayed x) =
  pretty x
prettyExpr _ (EDelay x) =
  pretty x
prettyExpr _ (EForce x) =
  pretty x
prettyExpr _ (ECase scrut alts) =
  keyword "case"
  <++>
    pretty scrut <++> keyword "of" `vappend` indent 2 (vsep (map pretty alts))
prettyExpr _ (ELocal decls scope) =
  let declsDoc = vsep (map pretty decls)
    in (keyword "let" `vappend` indent 2 declsDoc)
       `vappend`
         (keyword "in" <++> pretty scope)
prettyExpr _ (EList xs) =
  list (map pretty xs)
prettyExpr _ (ESnocList xs) =
  snocList (map pretty (xs <>> []))
prettyExpr _ (EPair x y) =
  lparen <+> pretty x <+> text ", " <+> pretty y <+> rparen
prettyExpr _ (EDPair l Nothing r) =
  parens (pretty l <++> keyword "**" <++> pretty r)
prettyExpr _ (EDPair l (Just ty) r) =
  parens (pretty l <++> colon <++> pretty ty <++> keyword "**" <++> pretty r)
prettyExpr _ (EString parts) =
  dquotes (hcat (map pretty parts))
prettyExpr _ (EDo _ stmts) =
  keyword "do" `vappend` indent 2 (vsep (map pretty stmts))
prettyExpr _ (EIdiom _ x) =
  lbracket <+> pipe <+> pretty x <+> pipe <+> rbracket
prettyExpr _ (EIf c t f) =
  let
    cond       = keyword "if" <++> pretty c
    thenBranch = branchDoc (keyword "then") t
    elseBranch = branchDoc (keyword "else") f
    horizontal = cond <++> thenBranch <++> elseBranch
    thenDoc    = cond `vappend` indent 2 thenBranch
    vertical   = thenDoc `vappend` indent 2 elseBranch
  in ifMultiline horizontal vertical
prettyExpr _ (EHole s) =
  line "?" <+> line s
prettyExpr _ EType =
  keyword "Type"
prettyExpr _ EUnit =
  line "()"
prettyExpr _ EImplicit =
  line "_"
prettyExpr _ (EQuote x) =
  line "`" <+> pretty x <+> line "`"
prettyExpr _ (EQuoteName n) =
  line "`" <+> pretty n
prettyExpr _ (EQuoteDecl ds) =
  line "`(" <+> vsep (map pretty ds) <+> line ")"
prettyExpr _ (EUnquote x) =
  line "~" <+> pretty x
prettyExpr _ (EPrim c) =
  pretty c
prettyExpr d (EOp l op r) =
  let
    horiz = prettyPrec Open l <++> pretty op <++> prettyPrec Open r
    vert  = vsep [prettyPrec Open l, pretty op, indent 2 (prettyPrec Open r)]
  in parenthesise (d >= App) $ ifMultiline horiz vert
prettyExpr _ (EPrefixOp op x) =
  pretty op <++> pretty x
prettyExpr _ (ESectionL op x) =
  parens (pretty op <++> pretty x)
prettyExpr _ (ESectionR x op) =
  parens (pretty x <++> pretty op)
prettyExpr _ (EBracketed x) =
  parens (pretty x)
prettyExpr _ (EAs n x) =
  pretty n <+> line "@" <+> pretty x
prettyExpr _ (ETyped expr ty) =
  pretty expr <++> line ":" <++> pretty ty
prettyExpr _ (EDotted x) =
  line "." <+> pretty x
prettyExpr _ (EComment c x) =
  pretty c `vappend` pretty x
prettyExpr _ (ERecordUpdate rec fields) =
  let fieldDocs = map (\(path, val) => line (concat (intersperse "." path)) <++> text ":=" <++> pretty val) fields
   in braces (hsep (intersperse (line ",") fieldDocs)) <++> pretty rec
prettyDecl _ (DModule name _) =
  keyword "module" <++> line name
prettyDecl _ (DImport imp) =
  pretty imp
prettyDecl _ (DClaim comments vis n ty fnOpts) =
  let
    fnOptsDoc = hsep (map fnOptDoc fnOpts)
    base      = case fnOpts of
                  [] =>
                    pretty n <++> colon <++> pretty ty
                  _ =>
                    fnOptsDoc <++> pretty n <++> colon <++> pretty ty
    full      = case vis of
                  Private =>
                    base
                  _ =>
                    visibilityDoc vis <++> base
  in withComments comments full
prettyDecl _ (DDef comments n clauses) =
  let body = vsep (map pretty clauses) in withComments comments body
prettyDecl _ (DData comments vis dd@(MkDataDecl _ _ _ cons)) =
  let
    header = pretty dd
    body   = case cons of
               [] =>
                 empty
               _ =>
                 indent 2 (vsep (map pretty cons))
    full   = header `vappend` body
  in case vis of
       Private =>
         withComments comments full
       _ =>
         withComments comments (visibilityDoc vis `vappend` full)
prettyDecl _ (DRecord comments vis rd@(MkRecordDecl _ _ conName fields)) =
  let
    header    = pretty rd
    conDoc    = case conName of
                  Nothing =>
                    []
                  Just c =>
                    [keyword "constructor" <++> pretty c]
    allFields = conDoc ++ map pretty fields
    body      = case allFields of
                  [] =>
                    empty
                  _ =>
                    indent 2 (vsep allFields)
    full      = header `vappend` body
  in case vis of
       Private =>
         withComments comments full
       _ =>
         withComments comments (visibilityDoc vis `vappend` full)
prettyDecl _ (DInterface comments vis id@(MkInterfaceDecl _ _ _ methods)) =
  let
    header = pretty id
    body   = case methods of
               [] =>
                 empty
               _ =>
                 indent 2 (vsep (map pretty methods))
    full   = header `vappend` body
  in case vis of
       Private =>
         withComments comments full
       _ =>
         withComments comments (visibilityDoc vis `vappend` full)
prettyDecl _ (DImpl _ vis impl@(MkImplDecl _ _ _ body)) =
  let full = case body of
               Nothing =>
                 pretty impl
               Just ds =>
                 pretty impl `vappend` indent 2 (vsep (map pretty ds))
    in case vis of
         Private =>
           full
         _ =>
           visibilityDoc vis `vappend` full
prettyDecl _ (DFixity fd) =
  pretty fd
prettyDecl _ (DNamespace ns decls) =
  keyword "namespace"
  <++>
    hsep (map line ns)
    <++>
      keyword "where" `vappend` indent 2 (vsep (map pretty decls))
prettyDecl _ (DMutual decls) =
  keyword "mutual" `vappend` indent 2 (vsep (map pretty decls))
prettyDecl _ (DParams params decls) =
  let header = keyword "parameters" <++> parens (hsep (map paramDoc params))
    in header
       `vappend`
         indent 2 (keyword "where" `vappend` indent 2 (vsep (map pretty decls)))
prettyDecl _ (DUsing usings decls) =
  keyword "using"
  <++>
    parens (hsep (map usingDoc usings))
    <++>
      keyword "where" `vappend` indent 2 (vsep (map pretty decls))
prettyDecl _ (DDirective s) =
  keyword "%" <+> line s
prettyDecl _ (DBuiltin bt n) =
  keyword "%builtin" <++> line bt <++> pretty n
prettyDecl _ (DTransform name lhs rhs) =
  hangSep' 2
    (keyword "%transform" <++> line name <++> pretty lhs <++> keyword "=")
    (pretty rhs)
prettyDecl _ (DRunElab tm) =
  keyword "%runElab" <++> pretty tm
prettyDecl _ (DComment c) =
  pretty c
prettyDecl _ (DBlank n) =
  vsep (replicate n (line ""))
prettyClause _ (MkClause lhs (EDo _ stmts) ws) =
  let body = (pretty lhs <++> keyword "=" <++> keyword "do")
             `vappend`
               indent 2 (vsep (map pretty stmts))
    in case ws of
         [] =>
           body
         _ =>
           body
           `vappend`
             indent 2
               (keyword "where" `vappend` indent 2 (vsep (map pretty ws)))
prettyClause _ (MkClause lhs rhs ws) =
  let body = (pretty lhs <++> keyword "=") `vappend` indent 2 (pretty rhs)
    in case ws of
         [] =>
           body
         _ =>
           body
           `vappend`
             indent 2
               (keyword "where" `vappend` indent 2 (vsep (map pretty ws)))
prettyClause _ (MkCaseClause lhs (EDo _ stmts)) =
  (pretty lhs <++> keyword "=>" <++> keyword "do")
  `vappend`
    indent 2 (vsep (map pretty stmts))
prettyClause _ (MkCaseClause lhs rhs) =
  (pretty lhs <++> keyword "=>") `vappend` indent 2 (pretty rhs)
prettyClause _ (MkWith lhs wps cs) =
  pretty lhs
  <++>
    keyword "with"
    <++>
      parens (hsep (map pretty wps)) `vappend` indent 2 (vsep (map pretty cs))
prettyClause _ (MkImposs lhs) =
  pretty lhs <++> keyword "impossible"
prettyDoStmt _ (DoExp tm) =
  pretty tm
prettyDoStmt _ (DoBind n rig ty tm) =
  hangSep' 2 (prettyRig rig <+> pretty n <+> tyDoc ty <++> keyword "<-") (pretty tm)
  where
    tyDoc : Maybe (AST.Expr AST.Name) -> Doc opts
    tyDoc Nothing =
      Doc.empty
    tyDoc (Just t) =
      space <+> colon <++> pretty t
prettyDoStmt _ (DoBindPat pat ty val _) =
  hangSep' 2 (pretty pat <+> tyDoc ty <++> keyword "<-") (pretty val)
  where
    tyDoc : Maybe (AST.Expr AST.Name) -> Doc opts
    tyDoc Nothing =
      Doc.empty
    tyDoc (Just t) =
      space <+> colon <++> pretty t
prettyDoStmt _ (DoLet n rig ty tm) =
  hangSep' 6 (keyword "let" <++> prettyRig rig <+> pretty n <+> tyDoc ty <++> equals) (pretty tm)
  where
    tyDoc : AST.Expr AST.Name -> Doc opts
    tyDoc AST.EImplicit =
      Doc.empty
    tyDoc t =
      space <+> colon <++> pretty t
prettyDoStmt _ (DoLetPat pat val _) =
  hangSep' 6 (keyword "let" <++> pretty pat <++> equals) (pretty val)
prettyDoStmt _ (DoRewrite rule) =
  keyword "rewrite" <++> pretty rule
prettyStringPart _ (StrLit s) =
  text s
prettyStringPart _ (StrInterp tm) =
  line "\\{" <+> pretty tm <+> line "}"

tyParamDoc : {opts : _} -> (AST.Name, AST.Expr AST.Name) -> Doc opts
tyParamDoc (p, AST.EImplicit) =
  pretty p
tyParamDoc (p, t) =
  parens (pretty p <++> colon <++> pretty t)
prettyConDecl _ (MkConDecl n ty) =
  conNameDoc n <++> colon <++> pretty ty
prettyFieldDecl _ (MkFieldDecl n ty) =
  pretty n <++> colon <++> pretty ty
prettyDataDecl _ (MkDataDecl n params ty cons) =
  let
    paramsDoc = hsep (map tyParamDoc params)
    base      = pretty n <++> colon <++> pretty ty <++> keyword "where"
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
  let paramsDoc = hsep (map tyParamDoc params)
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
  keyword (fixityStr fix)
  <++>
    line (show prec) <++> hsep (map (line . show) names)
prettyImportDecl _ (MkImportDecl reexport name alias _ _) =
  importDoc reexport name alias
prettyConstant _ (AST.CInt i) =
  line (show i)
prettyConstant _ (AST.CString s) =
  dquotes (text s)
prettyConstant _ (AST.CChar c) =
  squotes (line (showCharLit c))
prettyConstant _ (AST.CDouble d) =
  line (show d)
prettyConstant _ AST.CWorldVal =
  text "WorldVal"
prettyOpStr _ (AST.OpSymbols s) =
  D.operator_ s
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
  let
    opts     = D.toLayoutOpts cfg
    rendered = Doc.render opts (vsep (map pretty decls))
  in Align.applyAlignment cfg rendered
