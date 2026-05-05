module IdrisFmt.Monad
import Control.Monad.Identity
import Control.Monad.RWS
import IdrisFmt.Config as CFG
import Text.PrettyPrint.Bernardy.Core as Core

%default total

||| A trace entry recording why a formatting decision was made.
public export
record Trace where
  constructor MkTrace
  construct : String
  decision : String

||| The printing context (Reader environment).
public export
record PrintCtx where
  constructor MkPrintCtx
  baseConfig : CFG.Config
  prec       : Prec
  layoutOpts : LayoutOpts

||| The monad: RWS with PrintCtx environment, List Trace writer, () state.
public export
PrinterM : Type -> Type
PrinterM = RWS PrintCtx (List Trace) ()

||| Run a PrinterM computation with the given context.
export
runPrinterM : CFG.Config -> Prec -> LayoutOpts -> PrinterM a -> (a, (), List Trace)
runPrinterM cfg p opts m = runRWS (MkPrintCtx cfg p opts) () m

||| Evaluate a PrinterM computation, discarding state.
export
evalPrinterM : CFG.Config -> Prec -> LayoutOpts -> PrinterM a -> (a, List Trace)
evalPrinterM cfg p opts m = evalRWS (MkPrintCtx cfg p opts) () m

||| Get the current base config.
export
getConfig : PrinterM CFG.Config
getConfig = asks baseConfig

||| Get the current precedence.
export
getPrec : PrinterM Prec
getPrec = asks prec

||| Run a sub-computation with updated precedence.
export
withPrec : Prec -> PrinterM a -> PrinterM a
withPrec p = local (\ctx => { prec := p } ctx)

||| Add a trace entry.
export
trace : String -> String -> PrinterM ()
trace construct decision =
  tell [MkTrace construct decision]

||| Conditional tracing.
export
traceWhen : Bool -> String -> String -> PrinterM ()
traceWhen True c d = trace c d
traceWhen False _ _ = pure ()
