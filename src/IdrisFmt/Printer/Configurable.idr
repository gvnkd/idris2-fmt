module IdrisFmt.Printer.Configurable
import Data.List as L
import Data.String as S
import IdrisFmt.AST as AST
import IdrisFmt.Blocks as Blocks
import IdrisFmt.Comments as C
import IdrisFmt.Config as CFG
import IdrisFmt.Doc as D
import IdrisFmt.Measure as Measure
import IdrisFmt.Monad as M
import Text.PrettyPrint.Bernardy.Combinators
import Text.PrettyPrint.Bernardy.Interface
import Text.PrettyPrint.Bernardy.Core as Core
import Control.Monad.Identity
import Control.Monad.RWS

%default covering

||| Style for let bindings.
public export
data LetStyle = Inline | Auto | Block

||| Style for function arrows in type signatures.
public export
data ArrowStyle = Trailing | Leading

||| Style for if-then-else layout.
public export
data IfStyle = Compact | Indented

export
Show IfStyle where
  show Compact = "Compact"
  show Indented = "Indented"

export
Show LetStyle where
  show Inline = "Inline"
  show Auto = "Auto"
  show Block = "Block"

export
Show ArrowStyle where
  show Trailing = "Trailing"
  show Leading = "Leading"

||| Extended formatter configuration with layout rules.
public export
record FmtConfig where
  constructor MkFmtConfig
  base       : CFG.Config
  letStyle   : LetStyle
  arrowStyle : ArrowStyle
  ifStyle    : IfStyle

||| Default formatter configuration.
export
defaultFmtConfig : FmtConfig
defaultFmtConfig = MkFmtConfig CFG.defaultConfig Auto Trailing Compact

||| Get layout options from formatter config.
layoutOpts : FmtConfig -> LayoutOpts
layoutOpts cfg = D.toLayoutOpts cfg.base

||| Context for configurable printing.
public export
record Ctx where
  constructor MkCtx
  config : FmtConfig
  prec   : Prec
  opts   : LayoutOpts

||| Monadic type for configurable printing.
public export
ConfigurableM : Type -> Type
ConfigurableM = RWS Ctx (List M.Trace) ()

||| Run a configurable computation.
export
runConfigurableM : FmtConfig -> Prec -> LayoutOpts -> ConfigurableM a -> (a, (), List M.Trace)
runConfigurableM cfg p opts m = runRWS (MkCtx cfg p opts) () m

||| Get formatter config.
export
getFmtConfig : ConfigurableM FmtConfig
getFmtConfig = asks config

||| Get precedence.
export
getPrec : ConfigurableM Prec
getPrec = asks prec

||| Update precedence locally.
export
withPrec : Prec -> ConfigurableM a -> ConfigurableM a
withPrec p = local (\ctx => { prec := p } ctx)

||| Add trace.
export
trace : String -> String -> ConfigurableM ()
trace c d = tell [M.MkTrace c d]

-- ---------------------------------------------------------------------------
-- Pure helpers (no monad needed)
-- ---------------------------------------------------------------------------

prettyRig : AST.RigCount -> Doc opts
prettyRig AST.Rig0 = line "0 "
prettyRig AST.Rig1 = line "1 "
prettyRig AST.RigW = Doc.empty

isOperatorChar : Char -> Bool
isOperatorChar c = not (isAlpha c || isDigit c || c == '_' || c == '\'' || c == '"')

isOperatorName : AST.Name -> Bool
isOperatorName (AST.UN s) =
  case unpack s of
    []       => False
    (c::cs)  => isOperatorChar c && all isOperatorChar cs
isOperatorName _ = False

-- ---------------------------------------------------------------------------
-- Monadic pretty-printing core
-- ---------------------------------------------------------------------------

mutual
  ||| Pretty-print a name.
  export
  prettyNameM : {opts : _} -> AST.Name -> ConfigurableM (Doc opts)
  prettyNameM (AST.UN s) =
    if isOperatorName (AST.UN s)
      then pure (parens (D.ident s))
      else pure (D.ident s)
  prettyNameM (AST.MN s i) =
    pure (D.ident (s ++ "_" ++ show i))
  prettyNameM (AST.NS ns n) = do
    nDoc <- prettyNameM n
    pure (D.ident (concat (L.intersperse "." (reverse ns)) ++ ".") <+> nDoc)
  
  ||| Check if expression is heavy (forces vertical layout).
  export
  isHeavy : AST.Expr AST.Name -> Bool
  isHeavy (EIf _ _ _) = True
  isHeavy (ECase _ _) = True
  isHeavy (EOp _ _ r) = isHeavy r
  isHeavy _ = False
  
  -- ---------------------------------------------------------------------------
  -- Let formatting variants (config-driven)
  -- ---------------------------------------------------------------------------
  
  prettyInlineLet : {opts : _} -> AST.RigCount -> AST.Expr AST.Name -> AST.Expr AST.Name -> AST.Expr AST.Name -> AST.Expr AST.Name -> ConfigurableM (Doc opts)
  prettyInlineLet rig pat ty val scope = do
    patDoc <- prettyExprM pat
    valDoc <- prettyExprM val
    scopeDoc <- prettyExprM scope
    pure (keyword "let" <++> patDoc <++> equals <++> valDoc <++> keyword "in" <++> scopeDoc)
  
  prettyBlockLet : {opts : _} -> AST.RigCount -> AST.Expr AST.Name -> AST.Expr AST.Name -> AST.Expr AST.Name -> AST.Expr AST.Name -> ConfigurableM (Doc opts)
  prettyBlockLet rig pat ty val scope = do
    case Blocks.flattenELet (ELet rig pat ty val scope []) of
      Just (MkLetBlock bs sc, _) => do
        bindDocs <- traverse (\(r, p, t, v) => do
          pDoc <- prettyExprM p
          vDoc <- prettyExprM v
          pure (pDoc <++> equals <++> vDoc)) bs
        scDoc <- prettyExprM sc
        let letKw = keyword "let"
            inKw = keyword "in"
            body = vsep (map (indent 2) bindDocs)
        pure ((letKw `vappend` body) `vappend` (inKw <++> scDoc))
      Nothing => prettyInlineLet rig pat ty val scope
  
  prettyAutoLet : {opts : _} -> AST.RigCount -> AST.Expr AST.Name -> AST.Expr AST.Name -> AST.Expr AST.Name -> AST.Expr AST.Name -> ConfigurableM (Doc opts)
  prettyAutoLet rig pat ty val scope =
    case Blocks.flattenELet (ELet rig pat ty val scope []) of
      Just (MkLetBlock bs sc, _) =>
        if length bs == 1
          then prettyInlineLet rig pat ty val sc
          else prettyBlockLet rig pat ty val sc
      Nothing => prettyInlineLet rig pat ty val scope
  
  ||| Pretty-print an expression with full config awareness.
  export
  prettyExprM : {opts : _} -> AST.Expr AST.Name -> ConfigurableM (Doc opts)
  prettyExprM (ERef n) = prettyNameM n
  
  prettyExprM (EIf c t f) = do
    cfg <- getFmtConfig
    ifStyle <- asks (\ctx => case ctx.config of
                            MkFmtConfig _ _ _ style => style)
    trace "EIf" ("using style: " ++ show ifStyle)
    cDoc <- prettyExprM c
    tDoc <- prettyExprM t
    fDoc <- prettyExprM f
    let cond : Doc opts = keyword "if" <++> cDoc
        thenBr : Doc opts = keyword "then" <++> tDoc
        elseBr : Doc opts = keyword "else" <++> fDoc
        compact : Doc opts = cond <++> thenBr <++> elseBr
        indented : Doc opts = (cond `vappend` indent 2 thenBr) `vappend` indent 2 elseBr
    case ifStyle of
      Compact   => pure (ifMultiline compact indented)
      Indented  => pure indented
  
  prettyExprM e@(ELet rig pat ty val scope _) = do
    fmtCfg <- asks (\ctx => case ctx.config of
                           MkFmtConfig _ style _ _ => style)
    trace "ELet" ("let-style: " ++ show fmtCfg)
    case fmtCfg of
      Inline => prettyInlineLet rig pat ty val scope
      Auto   => prettyAutoLet rig pat ty val scope
      Block  => prettyBlockLet rig pat ty val scope
  
  prettyExprM (EOp l op r) = do
    lDoc <- prettyExprM l
    rDoc <- prettyExprM r
    opDoc <- prettyOpStrM op
    if isHeavy r
      then do
        trace "EOp" "rhs is heavy, vertical layout"
        pure (vsep [lDoc, opDoc, indent 2 rDoc])
      else
        pure (lDoc <++> opDoc <++> rDoc)
  
  prettyExprM (EApp f x) = do
    fDoc <- prettyExprM f
    xDoc <- prettyExprM x
    pure (fDoc <++> xDoc)
  
  prettyExprM (EPi rig info n arg ret) = do
    arrowStyle <- asks (\ctx => case ctx.config of
                              MkFmtConfig _ _ style _ => style)
    argDoc : Doc opts <- prettyParamM rig info n arg
    retDoc : Doc opts <- prettyExprM ret
    case arrowStyle of
      Trailing => pure (argDoc <++> line "->" <++> retDoc)
      Leading  => pure (argDoc `vappend` indent 2 (line "-> " <+> retDoc))

  prettyExprM (EDPair l Nothing r) = do
    lDoc <- prettyExprM l
    rDoc <- prettyExprM r
    pure (parens (lDoc <++> keyword "**" <++> rDoc))

  prettyExprM (EDPair l (Just ty) r) = do
    lDoc <- prettyExprM l
    tyDoc <- prettyExprM ty
    rDoc <- prettyExprM r
    pure (parens (lDoc <++> colon <++> tyDoc <++> keyword "**" <++> rDoc))

  prettyExprM (EQuoteName n) = do
    nDoc <- prettyNameM n
    pure (line "`" <+> nDoc)

  prettyExprM _ =
    pure (text "/* TODO */")
  
  -- ---------------------------------------------------------------------------
  -- Helpers
  -- ---------------------------------------------------------------------------
  
  prettyParamM : {opts : _} -> AST.RigCount -> AST.PiInfo (AST.Expr AST.Name) -> Maybe AST.Name -> AST.Expr AST.Name -> ConfigurableM (Doc opts)
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
  prettyParamM _ (DefImplicit _) _ arg =
    prettyExprM arg
  prettyParamM _ _ Nothing arg =
    prettyExprM arg
  
  prettyOpStrM : {opts : _} -> AST.OpStr AST.Name -> ConfigurableM (Doc opts)
  prettyOpStrM (AST.OpSymbols s) = pure (line s)
  prettyOpStrM (AST.Backticked n) = do
    nDoc <- prettyNameM n
    pure (line "`" <+> nDoc <+> line "`")
  
  
-- ---------------------------------------------------------------------------
-- Entry point with config
-- ---------------------------------------------------------------------------

||| Render an expression with full configuration and get traces.
export
renderWithConfig : FmtConfig -> AST.Expr AST.Name -> (String, List M.Trace)
renderWithConfig cfg expr =
  let opts = layoutOpts cfg
      (doc, _, traces) = runConfigurableM cfg Open opts (prettyExprM expr)
   in (Core.Doc.render opts doc, traces)

||| Render with default config (backward compat).
export
renderDefault : AST.Expr AST.Name -> String
renderDefault expr =
  fst (renderWithConfig defaultFmtConfig expr)
