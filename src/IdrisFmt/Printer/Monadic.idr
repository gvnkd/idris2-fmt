module IdrisFmt.Printer.Monadic
import Data.List as L
import Data.String as S
import IdrisFmt.AST as AST
import IdrisFmt.Config as CFG
import IdrisFmt.Doc as D
import IdrisFmt.Monad as M
import Text.PrettyPrint.Bernardy.Combinators
import Text.PrettyPrint.Bernardy.Interface
import Text.PrettyPrint.Bernardy.Core as Core
import Control.Monad.Identity
import Control.Monad.RWS

%default covering

||| Check if expression is heavy (forces vertical layout).
export
isHeavy : AST.Expr AST.Name -> Bool
isHeavy (EIf _ _ _) = True
isHeavy (ECase _ _) = True
isHeavy (EOp _ _ r) = isHeavy r
isHeavy _ = False

||| Monadic version of prettyName.
export
prettyNameM : {opts : _} -> AST.Name -> M.PrinterM (Doc opts)
prettyNameM (AST.UN s) =
  pure (D.ident s)
prettyNameM (AST.MN s i) =
  pure (D.ident (s ++ "_" ++ show i))
prettyNameM (AST.NS ns n) =
  pure (D.ident (concat (L.intersperse "." (reverse ns)) ++ ".") <+> D.ident (show n))

||| Monadic version of a subset of prettyExpr.
export
prettyExprM : {opts : _} -> AST.Expr AST.Name -> M.PrinterM (Doc opts)
prettyExprM (ERef n) =
  prettyNameM n
prettyExprM (EIf c t f) = do
  cfg <- M.getConfig
  M.trace "EIf" "formatting if-then-else"
  cDoc <- prettyExprM c
  tDoc <- prettyExprM t
  fDoc <- prettyExprM f
  let cond = keyword "if" <++> cDoc
      thenBranch = keyword "then" <++> tDoc
      elseBranch = keyword "else" <++> fDoc
      horizontal = cond <++> thenBranch <++> elseBranch
      vertical = (cond `vappend` indent 2 thenBranch) `vappend` indent 2 elseBranch
  pure (ifMultiline horizontal vertical)
prettyExprM (ELet rig pat ty val scope _) = do
  cfg <- M.getConfig
  M.trace "ELet" "formatting let binding"
  patDoc <- prettyExprM pat
  valDoc <- prettyExprM val
  scopeDoc <- prettyExprM scope
  let bindDoc = keyword "let" <++> patDoc <++> equals <++> valDoc
      inDoc = keyword "in" <++> scopeDoc
  pure (hangSep' 2 bindDoc inDoc)
prettyExprM (EOp l op r) = do
  cfg <- M.getConfig
  lDoc <- prettyExprM l
  rDoc <- prettyExprM r
  let opStr = case op of
                AST.OpSymbols s => s
                AST.Backticked n => "`" ++ show n ++ "`"
  if isHeavy r
    then do
      M.trace "EOp" "rhs is heavy, choosing vertical layout"
      pure (vsep [lDoc, line opStr, indent 2 rDoc])
    else do
      M.trace "EOp" "horizontal layout"
      pure (lDoc <++> line opStr <++> rDoc)
prettyExprM _ =
  pure (text "/* unimplemented */")

||| Render an expression with config and get traces.
export
renderExprWithTraces : CFG.Config -> Prec -> AST.Expr AST.Name -> (String, List M.Trace)
renderExprWithTraces cfg p expr =
  let opts = D.toLayoutOpts cfg
      (doc, _, traces) = M.runPrinterM cfg p opts (prettyExprM expr)
   in (Core.Doc.render opts doc, traces)
