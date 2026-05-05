module IdrisFmt.CLI
import Data.List as L
import Data.String as S
import IdrisFmt.Config as CFG
import Options.Applicative.Builder as B
import Options.Applicative.Run as R
import Options.Applicative.Types as T
import Options.Applicative.Help as H
import Options.Applicative.Multi as M
import System

%default covering

||| Parsed command-line arguments.
public export
record Args where
  constructor MkArgs
  files : List String
  config : CFG.Config
  check : Bool
  inplace : Bool
  stdin : Bool

||| Default args when no arguments provided.
defaultArgs : Args
defaultArgs = MkArgs [] CFG.defaultConfig False False False

||| Build the CLI parser.
cliParser : T.Parser Args
cliParser =
  MkArgs
  <$> filesP
  <*> configP
  <*> checkP
  <*> inplaceP
  <*> stdinP
  where
    checkP : T.Parser Bool
    checkP = flag' ["--check"] `H.mhelp` "Check formatting without writing"

    inplaceP : T.Parser Bool
    inplaceP = flag' ["--inplace"] `H.mhelp` "Edit files in place"

    stdinP : T.Parser Bool
    stdinP = flag' ["--stdin"] `H.mhelp` "Read from stdin"

    natOption : List String -> String -> T.Parser (Maybe Nat)
    natOption names desc =
      map parseNat
        (optionalStr
          (H.metavarMod (H.mhelp (strOption names) desc) "N"))
      where
        parseNat : Maybe String -> Maybe Nat
        parseNat Nothing = Nothing
        parseNat (Just s) = S.parsePositive s

        optionalStr : T.Parser String -> T.Parser (Maybe String)
        optionalStr p = map Just p <|> pure Nothing

    configP : T.Parser CFG.Config
    configP =
      mkConfig <$> natOption ["--indent"] "Indentation width (default: 2)"
                  <*> natOption ["--width"] "Line length (default: 80)"
      where
        mkConfig : Maybe Nat -> Maybe Nat -> CFG.Config
        mkConfig mIndent mWidth =
          let i = case mIndent of
                    Nothing => CFG.defaultConfig.indentWidth
                    Just n => n
              w = case mWidth of
                    Nothing => CFG.defaultConfig.lineLength
                    Just n => n
          in MkConfig i w CFG.defaultConfig.alignRules

    filesP : T.Parser (List String)
    filesP = M.manyUpTo 64 (argument "FILE" `H.mhelp` "Source files to format")

||| Check if args contain help flag.
isHelpFlag : List String -> Bool
isHelpFlag args = elem "--help" args || elem "-h" args

||| Run the CLI parser against raw arguments.
export parseArgs : List String -> Maybe Args
parseArgs [] =
  Nothing
parseArgs (prog :: args) =
  if isHelpFlag args
    then Nothing
    else case R.runParser cliParser args of
           T.Success val => Just val
           T.Failure _ =>
             Nothing
           T.CompletionInvoked =>
             Nothing

||| Usage string displayed on --help or invalid input.
export showUsage : String
showUsage =
  let helpInfo = H.collectHelpInfo "idris2-fmt" cliParser
  in H.formatHelp helpInfo
