module IdrisFmt.Printer.Complete
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
import Text.PrettyPrint.Bernardy.Core as Core
import Control.Monad.Identity
import Control.Monad.RWS

%default covering

||| A trace entry for formatting decisions.
public export
record Trace where
  constructor MkTrace
  construct : String
  decision : String

-- ---------------------------------------------------------------------------
-- Pure helpers (no monad needed)
-- ---------------------------------------------------------------------------

prettyRig : {layoutOpts : _} -> AST.RigCount -> Doc layoutOpts
prettyRig AST.Rig0 = line "0 "
prettyRig AST.Rig1 = line "1 "
prettyRig AST.RigW = Doc.empty

fixityStr : AST.Fixity -> String
fixityStr AST.InfixL = "infixl"
fixityStr AST.InfixR = "infixr"
fixityStr AST.Infix = "infix"
fixityStr AST.Prefix = "prefix"

isOperatorChar : Char -> Bool
isOperatorChar c = not (isAlpha c || isDigit c || c == '_' || c == '\'' || c == '"')

isOperatorName : AST.Name -> Bool
isOperatorName (AST.UN s) =
  case unpack s of
    [] => False
    (c :: cs) => isOperatorChar c && all isOperatorChar cs
isOperatorName _ = False

showCharLit : Char -> String
showCharLit '\n' = "\\n"
showCharLit '\t' = "\\t"
showCharLit '\r' = "\\r"
showCharLit '\\' = "\\\\"
showCharLit '\'' = "\\'"
showCharLit c = cast c

-- ---------------------------------------------------------------------------
-- Monadic context
-- ---------------------------------------------------------------------------

||| Style for let bindings.
public export
data LetStyle = Inline | Auto | Block

||| Style for function arrows.
public export
data ArrowStyle = Trailing | Leading

||| Style for if-then-else.
public export
data IfStyle = Compact | Indented

export
Show IfStyle where
  show Compact = "Compact"
  show Indented = "Indented"

export
Eq IfStyle where
  Compact == Compact = True
  Indented == Indented = True
  _ == _ = False

export
Show LetStyle where
  show Inline = "Inline"
  show Auto = "Auto"
  show Block = "Block"

export
Eq LetStyle where
  Inline == Inline = True
  Auto == Auto = True
  Block == Block = True
  _ == _ = False

export
Show ArrowStyle where
  show Trailing = "Trailing"
  show Leading = "Leading"

export
Eq ArrowStyle where
  Trailing == Trailing = True
  Leading == Leading = True
  _ == _ = False

||| Extended formatter config.
public export
record FmtConfig where
  constructor MkFmtConfig
  base       : CFG.Config
  letStyle   : LetStyle
  arrowStyle : ArrowStyle
  ifStyle    : IfStyle

export
defaultFmtConfig : FmtConfig
defaultFmtConfig = MkFmtConfig CFG.defaultConfig Auto Trailing Compact

layoutOpts : FmtConfig -> LayoutOpts
layoutOpts cfg = D.toLayoutOpts cfg.base

||| Context for monadic printing.
public export
record Ctx where
  constructor MkCtx
  config : FmtConfig
  prec   : Prec
  layoutOpts   : LayoutOpts

||| The monadic printer type.
public export
PrinterM : Type -> Type
PrinterM = RWS Ctx (List Trace) ()

||| Run a PrinterM computation.
export
runPrinterM : FmtConfig -> Prec -> LayoutOpts -> PrinterM a -> (a, (), List Trace)
runPrinterM cfg p layoutOpts m = runRWS (MkCtx cfg p layoutOpts) () m

||| Get formatter config.
export
getFmtConfig : PrinterM FmtConfig
getFmtConfig = asks config

||| Get precedence.
export
getPrec : PrinterM Prec
getPrec = asks prec

||| Update precedence locally.
export
withPrec : Prec -> PrinterM a -> PrinterM a
withPrec p = local (\ctx => { prec := p } ctx)

||| Add trace.
export
tr : String -> String -> PrinterM ()
tr c d = tell [MkTrace c d]

-- ---------------------------------------------------------------------------
-- Monadic helpers
-- ---------------------------------------------------------------------------

mutual
  export
  prettyNameM : {layoutOpts : _} -> AST.Name -> PrinterM (Doc layoutOpts)
  prettyNameM (AST.UN s) =
    if isOperatorName (AST.UN s)
      then pure (parens (D.ident s))
      else pure (D.ident s)
  prettyNameM (AST.MN s i) =
    pure (D.ident (s ++ "_" ++ show i))
  prettyNameM (AST.NS ns n) = do
    nDoc <- prettyNameM n
    pure (D.ident (concat (L.intersperse "." (reverse ns)) ++ ".") <+> nDoc)

  export
  prettyConstantM : {layoutOpts : _} -> AST.Constant -> PrinterM (Doc layoutOpts)
  prettyConstantM (AST.CInt i) = pure (line (show i))
  prettyConstantM (AST.CString s) = pure (dquotes (text s))
  prettyConstantM (AST.CChar c) = pure (squotes (line (showCharLit c)))
  prettyConstantM (AST.CDouble d) = pure (line (show d))
  prettyConstantM AST.CWorldVal = pure (text "WorldVal")

  export
  prettyOpStrM : {layoutOpts : _} -> AST.OpStr AST.Name -> PrinterM (Doc layoutOpts)
  prettyOpStrM (AST.OpSymbols s) = pure (line s)
  prettyOpStrM (AST.Backticked n) = do
    nDoc <- prettyNameM n
    pure (line "`" <+> nDoc <+> line "`")

  ||| Check if expression is heavy.
  export
  isHeavy : AST.Expr AST.Name -> Bool
  isHeavy (EIf _ _ _) = True
  isHeavy (ECase _ _) = True
  isHeavy (EOp _ _ r) = isHeavy r
  isHeavy _ = False

  ||| Pretty-print a parameter.
  prettyParamM : {layoutOpts : _} -> AST.RigCount -> AST.PiInfo (AST.Expr AST.Name) -> Maybe AST.Name -> AST.Expr AST.Name -> PrinterM (Doc layoutOpts)
  prettyParamM rig Explicit (Just n) arg = do
    nDoc <- prettyNameM n
    argDoc <- prettyExprM arg
    pure (parens (prettyRig rig <+> nDoc <++> colon <++> argDoc))
  prettyParamM rig Implicit (Just n) arg = do
    nDoc <- prettyNameM n
    argDoc <- prettyExprM arg
    pure (braces (prettyRig rig <+> nDoc <++> colon <++> argDoc))
  prettyParamM rig AutoImplicit (Just n) arg = do
    nDoc <- prettyNameM n
    argDoc <- prettyExprM arg
    pure (braces (keyword "auto" <++> prettyRig rig <+> nDoc <++> colon <++> argDoc))
  prettyParamM _ (DefImplicit _) _ arg = prettyExprM arg
  prettyParamM _ _ Nothing arg = prettyExprM arg

  -- Let formatting variants
  prettyInlineLetM : {layoutOpts : _} -> AST.RigCount -> AST.Expr AST.Name -> AST.Expr AST.Name -> AST.Expr AST.Name -> AST.Expr AST.Name -> PrinterM (Doc layoutOpts)
  prettyInlineLetM rig pat ty val scope = do
    patDoc <- prettyExprM pat
    valDoc <- prettyExprM val
    scopeDoc <- prettyExprM scope
    pure (keyword "let" <++> patDoc <++> equals <++> valDoc <++> keyword "in" <++> scopeDoc)

  prettyBlockLetM : {layoutOpts : _} -> List (AST.RigCount, (AST.Expr AST.Name, (AST.Expr AST.Name, AST.Expr AST.Name))) -> AST.Expr AST.Name -> PrinterM (Doc layoutOpts)
  prettyBlockLetM bs sc = do
    bindDocs <- traverse (\(r, p, t, v) => do
      pDoc <- prettyExprM p
      vDoc <- prettyExprM v
      pure (pDoc <++> equals <++> vDoc)) bs
    scDoc <- prettyExprM sc
    let letKw = keyword "let"
        inKw = keyword "in"
        body = vsep (map (indent 2) bindDocs)
    pure ((letKw `vappend` body) `vappend` (inKw <++> scDoc))

  prettyAutoLetM : {layoutOpts : _} -> AST.RigCount -> AST.Expr AST.Name -> AST.Expr AST.Name -> AST.Expr AST.Name -> AST.Expr AST.Name -> PrinterM (Doc layoutOpts)
  prettyAutoLetM rig pat ty val scope =
    case Blocks.flattenELet (ELet rig pat ty val scope []) of
      Just (MkLetBlock bs sc, _) =>
        if length bs == 1 && not (isHeavy val)
          then prettyInlineLetM rig pat ty val sc
          else prettyBlockLetM bs sc
      Nothing => prettyInlineLetM rig pat ty val scope

  -- Pi formatting variants
  prettySinglePiM : {layoutOpts : _} -> List (AST.RigCount, (AST.PiInfo (AST.Expr AST.Name), (Maybe AST.Name, AST.Expr AST.Name))) -> AST.Expr AST.Name -> PrinterM (Doc layoutOpts)
  prettySinglePiM [(rig, (info, (n, arg)))] res = do
    d <- getPrec
    arrowStyle <- asks (\ctx => case ctx.config of MkFmtConfig _ _ style _ => style)
    argDoc <- prettyParamM rig info n arg
    resDoc <- withPrec Open (prettyExprM res)
    let trailingDoc = hangSep' 2 argDoc (line "->" <++> resDoc)
        leadingDoc = vsep [argDoc, indent 2 (line "->" <++> resDoc)]
    if arrowStyle == Trailing
      then pure (parenthesise (d > Open) trailingDoc)
      else pure (parenthesise (d > Open) leadingDoc)
  prettySinglePiM _ res = withPrec Open (prettyExprM res)

  piParamDocsM : {layoutOpts : _} -> List (AST.RigCount, (AST.PiInfo (AST.Expr AST.Name), (Maybe AST.Name, AST.Expr AST.Name))) -> PrinterM (List (Doc layoutOpts))
  piParamDocsM [] = pure []
  piParamDocsM ((rig, (info, (n, arg))) :: rest) = do
    doc <- prettyParamM rig info n arg
    restDocs <- piParamDocsM rest
    pure (doc :: restDocs)

  piAlignedParamsM : {layoutOpts : _} -> Nat -> List (Doc layoutOpts) -> List (Doc layoutOpts)
  piAlignedParamsM _ [] = []
  piAlignedParamsM w (p :: rest) =
    (Measure.padTo w p <++> line "->") :: piAlignedParamsM w rest

  prettyPiBlockM : {layoutOpts : _} -> List (AST.RigCount, (AST.PiInfo (AST.Expr AST.Name), (Maybe AST.Name, AST.Expr AST.Name))) -> AST.Expr AST.Name -> PrinterM (Doc layoutOpts)
  prettyPiBlockM ps res = do
    d <- getPrec
    arrowStyle <- asks (\ctx => case ctx.config of MkFmtConfig _ _ style _ => style)
    paramDocs <- piParamDocsM ps
    let maxParamW = Measure.maxWidth (map Measure.measureWidth paramDocs)
        alignedParams = piAlignedParamsM maxParamW paramDocs
    resDoc <- withPrec Open (prettyExprM res)
    let vert = case paramDocs of
                 [] => resDoc
                 (p :: rest) => vsep (p :: map (\q => line "-> " <+> q) (rest ++ [resDoc]))
        horiz = hsep (intersperse (line "->") (paramDocs ++ [resDoc]))
        leadingParams = case paramDocs of
                          [] => [resDoc]
                          (p :: rest) => p :: map (\q => line "-> " <+> q) (rest ++ [resDoc])
    if arrowStyle == Trailing
      then pure (parenthesise (d > Open) (horiz <|> vert))
      else pure (parenthesise (d > Open) (vsep leadingParams))

  branchDocM : {layoutOpts : _} -> Doc layoutOpts -> AST.Expr AST.Name -> PrinterM (Doc layoutOpts)
  branchDocM kw (EDo _ stmts) = do
    stmtsDocs <- traverse prettyDoStmtM stmts
    pure (hangSep' 2 (kw <++> keyword "do") (vsep stmtsDocs))
  branchDocM kw (EIf c t f) = do
    ifDoc <- prettyExprM (EIf c t f)
    pure (kw `vappend` indent 2 ifDoc)
  branchDocM kw expr = do
    exprDoc <- prettyExprM expr
    pure (kw <++> exprDoc)

  ||| Pretty-print an expression.
  export
  prettyExprM : {layoutOpts : _} -> AST.Expr AST.Name -> PrinterM (Doc layoutOpts)
  prettyExprM (ERef n) = prettyNameM n

  prettyExprM e@(EPi _ _ _ _ _) =
    case Blocks.flattenEPi e of
      Just (MkPiBlock ps res) =>
        if length ps == 1
          then prettySinglePiM ps res
          else prettyPiBlockM ps res
      Nothing => prettyNameM (AST.UN "/* unflattened pi */")

  prettyExprM (EForall ns scope) = do
    d <- getPrec
    scopeDoc <- withPrec Open (prettyExprM scope)
    nsDocs <- traverse prettyNameM ns
    pure (parenthesise (d > Open) $ hangSep' 2 (keyword "forall" <++> hsep nsDocs <++> line ".") scopeDoc)

  prettyExprM (ELam rig _ pat ty (EDo _ stmts)) = do
    d <- getPrec
    binderDoc <- binderDocM rig pat ty
    stmtsDocs <- traverse prettyDoStmtM stmts
    let binder = keyword "\\" <+> binderDoc <++> line "=>" <++> keyword "do"
    pure (parenthesise (d > Open) $ hangSep' 2 binder (vsep stmtsDocs))

  prettyExprM (ELam rig _ pat ty scope) = do
    d <- getPrec
    binderDoc <- binderDocM rig pat ty
    scopeDoc <- withPrec Open (prettyExprM scope)
    let binder = keyword "\\" <+> binderDoc <++> line "=>"
    pure (parenthesise (d > Open) $ hangSep' 2 binder scopeDoc)

  prettyExprM (ELet rig pat ty val scope _) = do
    fmtCfg <- asks (\ctx => case ctx.config of MkFmtConfig _ style _ _ => style)
    tr "ELet" ("let-style: " ++ show fmtCfg)
    case fmtCfg of
      Inline => prettyInlineLetM rig pat ty val scope
      Auto   => prettyAutoLetM rig pat ty val scope
      Block  =>
        case Blocks.flattenELet (ELet rig pat ty val scope []) of
          Just (MkLetBlock bs sc, _) => prettyBlockLetM bs sc
          Nothing => prettyInlineLetM rig pat ty val scope

  prettyExprM (EApp f (EDo _ stmts)) = do
    d <- getPrec
    fDoc <- withPrec Open (prettyExprM f)
    stmtsDocs <- traverse prettyDoStmtM stmts
    pure (parenthesise (d >= App) $ hangSep' 2 (fDoc <++> keyword "do") (vsep stmtsDocs))

  prettyExprM (EApp f x) = do
    d <- getPrec
    fDoc <- withPrec Open (prettyExprM f)
    xDoc <- withPrec App (prettyExprM x)
    pure (parenthesise (d >= App) $ hangSep' 2 fDoc xDoc)

  prettyExprM (ENamedApp f n x) = do
    fDoc <- prettyExprM f
    nDoc <- prettyNameM n
    xDoc <- prettyExprM x
    pure (hangSep' 2 fDoc (braces (nDoc <++> equals <++> xDoc)))

  prettyExprM (EAutoApp f x) = do
    fDoc <- prettyExprM f
    xDoc <- prettyExprM x
    pure (hangSep' 2 fDoc xDoc)

  prettyExprM (EWithApp f x) = do
    fDoc <- prettyExprM f
    xDoc <- prettyExprM x
    pure (hangSep' 2 fDoc (keyword "with" <++> xDoc))

  prettyExprM (EPostfixApp rec fields) = do
    recDoc <- prettyExprM rec
    let fieldDocs = map (line . show) fields
    pure (recDoc <+> hcat (concatMap (\f => [line ".", f]) fieldDocs))

  prettyExprM (EPostfixAppPartial fields) =
    let fieldDocs = map (line . show) fields
    in pure (hcat (concatMap (\f => [line ".", f]) fieldDocs))

  prettyExprM (EDelayed x) = prettyExprM x
  prettyExprM (EDelay x) = prettyExprM x
  prettyExprM (EForce x) = prettyExprM x

  prettyExprM (ECase scrut alts) = do
    scrutDoc <- prettyExprM scrut
    altsDocs <- traverse prettyClauseM alts
    pure (keyword "case" <++> scrutDoc <++> keyword "of" `vappend` indent 2 (vsep altsDocs))

  prettyExprM (ELocal decls scope) = do
    declsDocs <- traverse prettyDeclM decls
    scopeDoc <- prettyExprM scope
    let declsDoc = vsep declsDocs
    pure ((keyword "let" `vappend` indent 2 declsDoc) `vappend` (keyword "in" <++> scopeDoc))

  prettyExprM (EList xs) = do
    xsDocs <- traverse prettyExprM xs
    pure (list xsDocs)

  prettyExprM (ESnocList xs) = do
    let xs' = xs <>> []
    xsDocs <- traverse prettyExprM xs'
    pure (snocList xsDocs)

  prettyExprM (EPair x y) = do
    xDoc <- withPrec Open (prettyExprM x)
    yDoc <- withPrec Open (prettyExprM y)
    pure (lparen <+> xDoc <+> text ", " <+> yDoc <+> rparen)

  prettyExprM (EString parts) = do
    partsDocs <- traverse prettyStringPartM parts
    pure (dquotes (hcat partsDocs))

  prettyExprM (EDo _ stmts) = do
    stmtsDocs <- traverse prettyDoStmtM stmts
    pure (keyword "do" `vappend` indent 2 (vsep stmtsDocs))

  prettyExprM (EIdiom _ x) = do
    xDoc <- prettyExprM x
    pure (lbracket <+> pipe <+> xDoc <+> pipe <+> rbracket)

  prettyExprM (EIf c t f) = do
    ifStyle <- asks (\ctx => case ctx.config of MkFmtConfig _ _ _ style => style)
    tr "EIf" ("using style: " ++ show ifStyle)
    cDoc <- prettyExprM c
    tDoc <- branchDocM (keyword "then") t
    fDoc <- branchDocM (keyword "else") f
    let cond : Doc layoutOpts = keyword "if" <++> cDoc
        compact : Doc layoutOpts = cond <++> tDoc <++> fDoc
        indented : Doc layoutOpts = (cond `vappend` indent 2 tDoc) `vappend` indent 2 fDoc
    case ifStyle of
      Compact  => pure (ifMultiline compact indented)
      Indented => pure indented

  prettyExprM (EHole s) = pure (line "?" <+> line s)
  prettyExprM EType = pure (keyword "Type")
  prettyExprM EUnit = pure (line "()")
  prettyExprM EImplicit = pure (line "_")

  prettyExprM (EQuote x) = do
    xDoc <- prettyExprM x
    pure (line "`" <+> xDoc <+> line "`")

  prettyExprM (EUnquote x) = do
    xDoc <- prettyExprM x
    pure (line "~" <+> xDoc)

  prettyExprM (EPrim c) = prettyConstantM c

  prettyExprM (EOp l op r) = do
    d <- getPrec
    lDoc <- withPrec Open (prettyExprM l)
    rDoc <- withPrec Open (prettyExprM r)
    opDoc <- prettyOpStrM op
    let horiz = lDoc <++> opDoc <++> rDoc
        vert = vsep [lDoc <++> opDoc, indent 2 rDoc]
    if isHeavy r
      then do
        tr "EOp" "rhs is heavy, vertical layout"
        pure (parenthesise (d >= App) vert)
      else
        pure (parenthesise (d >= App) (ifMultiline horiz vert))

  prettyExprM (EPrefixOp op x) = do
    opDoc <- prettyOpStrM op
    xDoc <- prettyExprM x
    pure (opDoc <++> xDoc)

  prettyExprM (ESectionL op x) = do
    opDoc <- prettyOpStrM op
    xDoc <- prettyExprM x
    pure (parens (opDoc <++> xDoc))

  prettyExprM (ESectionR x op) = do
    xDoc <- prettyExprM x
    opDoc <- prettyOpStrM op
    pure (parens (xDoc <++> opDoc))

  prettyExprM (EBracketed x) = do
    xDoc <- withPrec Open (prettyExprM x)
    pure (parens xDoc)

  prettyExprM (EAs n x) = do
    nDoc <- prettyNameM n
    xDoc <- prettyExprM x
    pure (nDoc <+> line "@" <+> xDoc)

  prettyExprM (EDotted x) = do
    xDoc <- prettyExprM x
    pure (line "." <+> xDoc)

  prettyExprM (EComment c x) = do
    cDoc <- prettyCommentM c
    xDoc <- prettyExprM x
    pure (cDoc `vappend` xDoc)

  prettyExprM (ERecordUpdate rec fields) = do
    recDoc <- prettyExprM rec
    fieldDocs <- traverse (\(path, val) => do
      valDoc <- prettyExprM val
      pure (line (concat (intersperse "." path)) <++> text ":=" <++> valDoc)) fields
    pure (braces (hsep (intersperse (line ",") fieldDocs)) <++> recDoc)

  -- Helper for binder docs
  binderDocM : {layoutOpts : _} -> AST.RigCount -> AST.Expr AST.Name -> AST.Expr AST.Name -> PrinterM (Doc layoutOpts)
  binderDocM r p AST.EImplicit = do
    pDoc <- prettyExprM p
    pure (prettyRig r <+> pDoc)
  binderDocM r p t = do
    pDoc <- prettyExprM p
    tDoc <- prettyExprM t
    pure (prettyRig r <+> pDoc <++> colon <++> tDoc)

  -- ---------------------------------------------------------------------------
  -- Declarations
  -- ---------------------------------------------------------------------------

  export
  prettyDeclM : {layoutOpts : _} -> AST.Decl AST.Name -> PrinterM (Doc layoutOpts)
  prettyDeclM (DModule name _) = pure (keyword "module" <++> line name)
  prettyDeclM (DImport imp) = prettyImportDeclM imp

  prettyDeclM (DClaim comments vis n ty fnOpts) = do
    fnOptsDoc <- pure (hsep (map fnOptDocM fnOpts))
    nDoc <- prettyNameM n
    tyDoc <- prettyExprM ty
    let base = case fnOpts of
                 [] => nDoc <++> colon <++> tyDoc
                 _  => fnOptsDoc <++> nDoc <++> colon <++> tyDoc
        full = case vis of
                 Private => base
                 _       => visibilityDocM vis <++> base
    withCommentsM comments full

  prettyDeclM (DDef comments n clauses) = do
    clausesDocs <- traverse prettyClauseM clauses
    let body : Doc layoutOpts = vsep clausesDocs
    withCommentsM comments body

  prettyDeclM (DData comments vis dd@(MkDataDecl _ _ _ cons)) = do
    ddDoc <- prettyDataDeclM dd
    consDocs <- traverse prettyConDeclM cons
    let body = case consDocs of
                 [] => empty
                 _  => indent 2 (vsep consDocs)
        full = ddDoc `vappend` body
    case vis of
      Private => withCommentsM comments full
      _       => withCommentsM comments (visibilityDocM vis `vappend` full)

  prettyDeclM (DRecord comments vis rd@(MkRecordDecl _ _ conName fields)) = do
    rdDoc <- prettyRecordDeclM rd
    conDoc <- case conName of
                Nothing => pure []
                Just c  => do
                  cDoc <- prettyNameM c
                  pure [keyword "constructor" <++> cDoc]
    fieldsDocs <- traverse prettyFieldDeclM fields
    let allFields = conDoc ++ fieldsDocs
        body = case allFields of
                 [] => empty
                 _  => indent 2 (vsep allFields)
        full = rdDoc `vappend` body
    case vis of
      Private => withCommentsM comments full
      _       => withCommentsM comments (visibilityDocM vis `vappend` full)

  prettyDeclM (DInterface comments vis id@(MkInterfaceDecl _ _ _ methods)) = do
    idDoc <- prettyInterfaceDeclM id
    methodsDocs : List (Doc layoutOpts) <- traverse prettyDeclM methods
    let body : Doc layoutOpts = case methodsDocs of
                                  [] => empty
                                  _  => indent 2 (vsep methodsDocs)
    let full : Doc layoutOpts = idDoc `vappend` body
    case vis of
      Private => withCommentsM comments full
      _       => withCommentsM comments (visibilityDocM vis `vappend` full)

  prettyDeclM (DImpl comments vis impl@(MkImplDecl _ _ _ body)) = do
    implDoc : Doc layoutOpts <- prettyImplDeclM impl
    case body of
      Nothing => pure implDoc
      Just ds => do
        dsDocs <- traverse prettyDeclM ds
        let full : Doc layoutOpts = implDoc `vappend` indent 2 (vsep dsDocs)
        case vis of
          Private => pure full
          _       => pure (visibilityDocM vis `vappend` full)

  prettyDeclM (DFixity fd) = prettyFixityDeclM fd
  prettyDeclM (DNamespace ns decls) = do
    declsDocs <- traverse prettyDeclM decls
    pure (keyword "namespace" <++> hsep (map line ns) <++> keyword "where" `vappend` indent 2 (vsep declsDocs))
  prettyDeclM (DMutual decls) = do
    declsDocs <- traverse prettyDeclM decls
    pure (keyword "mutual" `vappend` indent 2 (vsep declsDocs))
  prettyDeclM (DParams params decls) = do
    paramsDocs <- traverse paramDocM params
    declsDocs <- traverse prettyDeclM decls
    let header = keyword "parameters" <++> parens (hsep paramsDocs)
    pure (header `vappend` indent 2 (keyword "where" `vappend` indent 2 (vsep declsDocs)))
  prettyDeclM (DUsing usings decls) = do
    usingsDocs <- traverse usingDocM usings
    declsDocs <- traverse prettyDeclM decls
    pure (keyword "using" <++> parens (hsep usingsDocs) <++> keyword "where" `vappend` indent 2 (vsep declsDocs))
  prettyDeclM (DDirective s) = pure (keyword "%" <+> line s)
  prettyDeclM (DBuiltin bt n) = do
    nDoc <- prettyNameM n
    pure (keyword "%builtin" <++> line bt <++> nDoc)
  prettyDeclM (DTransform name lhs rhs) = do
    lhsDoc <- prettyExprM lhs
    rhsDoc <- prettyExprM rhs
    pure (hangSep' 2 (keyword "%transform" <++> line name <++> lhsDoc <++> keyword "=") rhsDoc)
  prettyDeclM (DRunElab tm) = do
    tmDoc <- prettyExprM tm
    pure (keyword "%runElab" <++> tmDoc)
  prettyDeclM (DComment c) = prettyCommentM c
  prettyDeclM (DBlank n) = pure (vsep (replicate n (line "")))

  -- ---------------------------------------------------------------------------
  -- Clauses
  -- ---------------------------------------------------------------------------

  export
  prettyClauseM : {layoutOpts : _} -> AST.Clause AST.Name -> PrinterM (Doc layoutOpts)
  prettyClauseM (MkClause lhs (EDo _ stmts) ws) = do
    lhsDoc <- prettyExprM lhs
    stmtsDocs <- traverse prettyDoStmtM stmts
    let body = (lhsDoc <++> keyword "=" <++> keyword "do") `vappend` indent 2 (vsep stmtsDocs)
    wsDocs <- traverse prettyDeclM ws
    case ws of
      [] => pure body
      _  => pure (body `vappend` indent 2 (keyword "where" `vappend` indent 2 (vsep wsDocs)))

  prettyClauseM (MkClause lhs rhs ws) = do
    lhsDoc <- prettyExprM lhs
    rhsDoc <- prettyExprM rhs
    let body = (lhsDoc <++> keyword "=") `vappend` indent 2 rhsDoc
    case ws of
      [] => pure body
      _  => do
        wsDocs <- traverse prettyDeclM ws
        pure (body `vappend` indent 2 (keyword "where" `vappend` indent 2 (vsep wsDocs)))

  prettyClauseM (MkCaseClause lhs (EDo _ stmts)) = do
    lhsDoc <- prettyExprM lhs
    stmtsDocs <- traverse prettyDoStmtM stmts
    pure ((lhsDoc <++> keyword "=>" <++> keyword "do") `vappend` indent 2 (vsep stmtsDocs))

  prettyClauseM (MkCaseClause lhs rhs) = do
    lhsDoc <- prettyExprM lhs
    rhsDoc <- prettyExprM rhs
    let horiz = lhsDoc <++> keyword "=>" <++> rhsDoc
        vert = (lhsDoc <++> keyword "=>") `vappend` indent 2 rhsDoc
    pure (ifMultiline horiz vert)

  prettyClauseM (MkWith lhs wps cs) = do
    lhsDoc <- prettyExprM lhs
    wpsDocs <- traverse prettyExprM wps
    csDocs <- traverse prettyClauseM cs
    pure (lhsDoc <++> keyword "with" <++> parens (hsep wpsDocs) `vappend` indent 2 (vsep csDocs))

  prettyClauseM (MkImposs lhs) = do
    lhsDoc <- prettyExprM lhs
    pure (lhsDoc <++> keyword "impossible")

  -- ---------------------------------------------------------------------------
  -- Do statements
  -- ---------------------------------------------------------------------------

  tyDocM : {layoutOpts : _} -> Maybe (AST.Expr AST.Name) -> PrinterM (Doc layoutOpts)
  tyDocM Nothing = pure Doc.empty
  tyDocM (Just t) = do
    tDoc <- prettyExprM t
    pure (space <+> colon <++> tDoc)

  export
  prettyDoStmtM : {layoutOpts : _} -> AST.DoStmt AST.Name -> PrinterM (Doc layoutOpts)
  prettyDoStmtM (DoExp tm) = prettyExprM tm
  prettyDoStmtM (DoBind n rig ty tm) = do
    nDoc <- prettyNameM n
    tmDoc <- prettyExprM tm
    tyDoc <- tyDocM ty
    pure ((prettyRig rig <+> nDoc <+> tyDoc <++> keyword "<-") `vappend` indent 2 tmDoc)

  prettyDoStmtM (DoBindPat pat ty val _) = do
    patDoc <- prettyExprM pat
    valDoc <- prettyExprM val
    tyDoc <- tyDocM ty
    pure (hangSep' 2 (patDoc <+> tyDoc <++> keyword "<-") valDoc)

  prettyDoStmtM (DoLet n rig tm) = do
    nDoc <- prettyNameM n
    tmDoc <- prettyExprM tm
    pure (hangSep' 6 (keyword "let" <++> prettyRig rig <+> nDoc <++> equals) tmDoc)

  prettyDoStmtM (DoLetPat pat val _) = do
    patDoc <- prettyExprM pat
    valDoc <- prettyExprM val
    pure (hangSep' 6 (keyword "let" <++> patDoc <++> equals) valDoc)

  prettyDoStmtM (DoRewrite rule) = do
    ruleDoc <- prettyExprM rule
    pure (keyword "rewrite" <++> ruleDoc)

  -- ---------------------------------------------------------------------------
  -- String parts
  -- ---------------------------------------------------------------------------

  export
  prettyStringPartM : {layoutOpts : _} -> AST.StringPart AST.Name -> PrinterM (Doc layoutOpts)
  prettyStringPartM (StrLit s) = pure (text s)
  prettyStringPartM (StrInterp tm) = do
    tmDoc <- prettyExprM tm
    pure (line "\\{" <+> tmDoc <+> line "}")

  -- ---------------------------------------------------------------------------
  -- Constructor declarations
  -- ---------------------------------------------------------------------------

  export
  prettyConDeclM : {layoutOpts : _} -> AST.ConDecl AST.Name -> PrinterM (Doc layoutOpts)
  prettyConDeclM (MkConDecl n ty) = do
    nDoc <- prettyNameM n
    tyDoc <- prettyExprM ty
    pure (nDoc <++> colon <++> tyDoc)

  -- ---------------------------------------------------------------------------
  -- Field declarations
  -- ---------------------------------------------------------------------------

  export
  prettyFieldDeclM : {layoutOpts : _} -> AST.FieldDecl AST.Name -> PrinterM (Doc layoutOpts)
  prettyFieldDeclM (MkFieldDecl n ty) = do
    nDoc <- prettyNameM n
    tyDoc <- prettyExprM ty
    pure (nDoc <++> colon <++> tyDoc)

  -- ---------------------------------------------------------------------------
  -- Data declarations
  -- ---------------------------------------------------------------------------

  export
  prettyDataDeclM : {layoutOpts : _} -> AST.DataDecl AST.Name -> PrinterM (Doc layoutOpts)
  prettyDataDeclM (MkDataDecl n params ty cons) = do
    nDoc <- prettyNameM n
    tyDoc <- prettyExprM ty
    paramsDocs <- traverse tyParamDocM params
    let paramsDoc = hsep paramsDocs
        base = nDoc <++> colon <++> tyDoc <++> keyword "where"
    pure (if null params
            then keyword "data" <++> base
            else hsep [keyword "data", nDoc, paramsDoc, colon, tyDoc, keyword "where"])

  -- ---------------------------------------------------------------------------
  -- Record declarations
  -- ---------------------------------------------------------------------------

  export
  prettyRecordDeclM : {layoutOpts : _} -> AST.RecordDecl AST.Name -> PrinterM (Doc layoutOpts)
  prettyRecordDeclM (MkRecordDecl n params conName fields) = do
    nDoc <- prettyNameM n
    paramsDocs <- traverse tyParamDocM params
    let paramsDoc = hsep paramsDocs
    pure (if null params
            then keyword "record" <++> nDoc <++> keyword "where"
            else hsep [keyword "record", nDoc, paramsDoc, keyword "where"])

  -- ---------------------------------------------------------------------------
  -- Interface declarations
  -- ---------------------------------------------------------------------------

  export
  prettyInterfaceDeclM : {layoutOpts : _} -> AST.InterfaceDecl AST.Name -> PrinterM (Doc layoutOpts)
  prettyInterfaceDeclM (MkInterfaceDecl n params _ methods) = do
    nDoc <- prettyNameM n
    paramsDocs <- traverse interfaceParamDocM params
    let paramsDoc = hsep paramsDocs
    pure (if null params
            then keyword "interface" <++> nDoc <++> keyword "where"
            else hsep [keyword "interface", nDoc, paramsDoc, keyword "where"])

  -- ---------------------------------------------------------------------------
  -- Implementation declarations
  -- ---------------------------------------------------------------------------

  export
  prettyImplDeclM : {layoutOpts : _} -> AST.ImplDecl AST.Name -> PrinterM (Doc layoutOpts)
  prettyImplDeclM (MkImplDecl name interfaceName params body) = do
    interfaceDoc <- prettyNameM interfaceName
    paramsDocs <- traverse prettyExprM params
    nameDocs : List (Doc layoutOpts) <- case name of
                  Nothing => pure []
                  Just n  => do
                    nDoc <- prettyNameM n
                    pure [nDoc, equals]
    let header : Doc layoutOpts = hsep (keyword "implementation" :: nameDocs ++ [interfaceDoc] ++ paramsDocs)
    case body of
      Nothing => pure header
      Just _  => pure (hangSep' 2 header (keyword "where"))

  -- ---------------------------------------------------------------------------
  -- Fixity declarations
  -- ---------------------------------------------------------------------------

  export
  prettyFixityDeclM : {layoutOpts : _} -> AST.FixityDecl -> PrinterM (Doc layoutOpts)
  prettyFixityDeclM (MkFixityDecl fix prec names) = do
    let namesDocs = map (line . show) names
    pure (keyword (fixityStr fix) <++> line (show prec) <++> hsep namesDocs)

  -- ---------------------------------------------------------------------------
  -- Import declarations
  -- ---------------------------------------------------------------------------

  export
  prettyImportDeclM : {layoutOpts : _} -> AST.ImportDecl -> PrinterM (Doc layoutOpts)
  prettyImportDeclM (MkImportDecl reexport name alias _ _) =
    let pub = if reexport then [keyword "public"] else []
        modName = line (concat (intersperse "." name))
        asDoc = case alias of
                  Nothing => []
                  Just a  => [keyword "as" <++> line a]
    in pure (hsep ([keyword "import"] ++ pub ++ [modName] ++ asDoc))

  -- ---------------------------------------------------------------------------
  -- Comments
  -- ---------------------------------------------------------------------------

  export
  prettyCommentM : {layoutOpts : _} -> C.Comment -> PrinterM (Doc layoutOpts)
  prettyCommentM (C.MkComment C.LineComment content _ _) = pure (text "-- " <+> text content)
  prettyCommentM (C.MkComment C.BlockComment content _ _) = pure (text ("{- " ++ content ++ " -}"))
  prettyCommentM (C.MkComment C.DocComment content _ _) = pure (text "||| " <+> text content)

  -- ---------------------------------------------------------------------------
  -- More pure helpers (need monadic versions)
  -- ---------------------------------------------------------------------------

  fnOptDocM : {layoutOpts : _} -> AST.FnOpt -> Doc layoutOpts
  fnOptDocM AST.Inline = keyword "%inline"
  fnOptDocM AST.TCInline = keyword "%tcinline"
  fnOptDocM AST.NoInline = keyword "%noinline"

  paramDocM : {layoutOpts : _} -> (AST.Name, Maybe (AST.Expr AST.Name)) -> PrinterM (Doc layoutOpts)
  paramDocM (n, Nothing) = prettyNameM n
  paramDocM (n, Just ty) = do
    nDoc <- prettyNameM n
    tyDoc <- prettyExprM ty
    pure (nDoc <++> colon <++> tyDoc)

  usingDocM : {layoutOpts : _} -> (Maybe AST.Name, AST.Expr AST.Name) -> PrinterM (Doc layoutOpts)
  usingDocM (Nothing, ty) = prettyExprM ty
  usingDocM (Just n, ty) = do
    nDoc <- prettyNameM n
    tyDoc <- prettyExprM ty
    pure (nDoc <++> colon <++> tyDoc)

  visibilityDocM : {layoutOpts : _} -> AST.Visibility -> Doc layoutOpts
  visibilityDocM AST.Private = empty
  visibilityDocM AST.Export = keyword "export"
  visibilityDocM AST.Public = keyword "public" <++> keyword "export"

  interfaceParamDocM : {layoutOpts : _} -> (AST.Name, AST.Expr AST.Name) -> PrinterM (Doc layoutOpts)
  interfaceParamDocM (p, AST.EImplicit) = prettyNameM p
  interfaceParamDocM (p, ty) = do
    pDoc <- prettyNameM p
    tyDoc <- prettyExprM ty
    pure (parens (pDoc <++> colon <++> tyDoc))

  tyParamDocM : {layoutOpts : _} -> (AST.Name, AST.Expr AST.Name) -> PrinterM (Doc layoutOpts)
  tyParamDocM (p, AST.EImplicit) = prettyNameM p
  tyParamDocM (p, t) = do
    pDoc <- prettyNameM p
    tDoc <- prettyExprM t
    pure (parens (pDoc <++> colon <++> tDoc))

  withCommentsM : {layoutOpts : _} -> List C.Comment -> Doc layoutOpts -> PrinterM (Doc layoutOpts)
  withCommentsM [] base = pure base
  withCommentsM cs base = do
    csDocs <- traverse prettyCommentM cs
    pure (vsep csDocs `vappend` base)

-- ---------------------------------------------------------------------------
-- Entry points
-- ---------------------------------------------------------------------------

||| Render an expression with config.
export
renderExprWithConfig : FmtConfig -> AST.Expr AST.Name -> (String, List Trace)
renderExprWithConfig cfg expr =
  let layoutOpts = layoutOpts cfg
      (doc, _, traces) = runPrinterM cfg Open layoutOpts (prettyExprM expr)
   in (Core.Doc.render layoutOpts doc, traces)

||| Render a module with config.
export
printModuleM : FmtConfig -> List (AST.Decl AST.Name) -> String
printModuleM cfg decls =
  let layoutOpts = layoutOpts cfg
      (doc, _, traces) = runPrinterM cfg Open layoutOpts (do
        declsDocs <- traverse prettyDeclM decls
        pure (vsep declsDocs))
      rendered = Core.Doc.render layoutOpts doc
   in Align.applyAlignment cfg.base rendered

||| Render with default config.
export
renderDefault : AST.Expr AST.Name -> String
renderDefault expr =
  fst (renderExprWithConfig defaultFmtConfig expr)
