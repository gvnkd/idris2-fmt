module IdrisFmt.CLI
import Data.List as L
import Data.String as S
import IdrisFmt.Config as CFG
import IdrisFmt.Printer.Complete as PRM
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
  letStyle : PRM.LetStyle
  arrowStyle : PRM.ArrowStyle
  ifStyle : PRM.IfStyle
  check : Bool
  inplace : Bool
  stdin : Bool

||| Default args when no arguments provided.
defaultArgs : Args
defaultArgs = MkArgs [] CFG.defaultConfig PRM.Auto PRM.Trailing PRM.Compact False False False

||| Known boolean flags.
knownFlags : List String
knownFlags = ["--check", "--inplace", "--stdin"]

||| Known options that take a value.
knownOptions : List String
knownOptions = ["--indent", "--width", "--let-style", "--arrow-style", "--if-style"]

||| Parse a LetStyle from string.
parseLetStyle : String -> Maybe PRM.LetStyle
parseLetStyle "Inline" = Just PRM.Inline
parseLetStyle "Auto" = Just PRM.Auto
parseLetStyle "Block" = Just PRM.Block
parseLetStyle _ = Nothing

||| Parse an ArrowStyle from string.
parseArrowStyle : String -> Maybe PRM.ArrowStyle
parseArrowStyle "Trailing" = Just PRM.Trailing
parseArrowStyle "Leading" = Just PRM.Leading
parseArrowStyle _ = Nothing

||| Parse an IfStyle from string.
parseIfStyle : String -> Maybe PRM.IfStyle
parseIfStyle "Compact" = Just PRM.Compact
parseIfStyle "Indented" = Just PRM.Indented
parseIfStyle _ = Nothing

||| Scan raw args into flags, options, and positional files.
scanArgs : List String -> (List String, List (String, String), List String)
scanArgs [] = ([], [], [])
scanArgs (arg :: rest) =
  if arg `elem` knownFlags
    then let (fs, os, ps) = scanArgs rest in (arg :: fs, os, ps)
    else if arg `elem` knownOptions
      then case rest of
             (val :: rest') =>
               let (fs, os, ps) = scanArgs rest' in (fs, (arg, val) :: os, ps)
             [] =>
               let (fs, os, ps) = scanArgs rest in (fs, os, arg :: ps)
      else let (fs, os, ps) = scanArgs rest in (fs, os, arg :: ps)

||| Build config from scanned options.
mkConfigFromOpts : List (String, String) -> CFG.Config
mkConfigFromOpts opts =
  let findOpt : String -> Maybe String
      findOpt name = lookup name opts
      mIndent = findOpt "--indent" >>= S.parsePositive
      mWidth = findOpt "--width" >>= S.parsePositive
      i = case mIndent of
            Nothing => CFG.defaultConfig.indentWidth
            Just n => fromInteger n
      w = case mWidth of
            Nothing => CFG.defaultConfig.lineLength
            Just n => fromInteger n
   in MkConfig i w CFG.defaultConfig.alignRules

||| Extract style options from scanned args.
mkStylesFromOpts : List (String, String) -> (PRM.LetStyle, PRM.ArrowStyle, PRM.IfStyle)
mkStylesFromOpts opts =
  let findOpt : String -> Maybe String
      findOpt name = lookup name opts
      ls = case findOpt "--let-style" >>= parseLetStyle of
             Nothing => PRM.Auto
             Just s => s
      as = case findOpt "--arrow-style" >>= parseArrowStyle of
             Nothing => PRM.Trailing
             Just s => s
      is = case findOpt "--if-style" >>= parseIfStyle of
             Nothing => PRM.Compact
             Just s => s
   in (ls, as, is)

||| Build the CLI parser (kept for help generation).
cliParser : T.Parser Args
cliParser =
  MkArgs
  <$> filesP
  <*> configP
  <*> pure PRM.Auto
  <*> pure PRM.Trailing
  <*> pure PRM.Compact
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
    else
      let (flags, opts, files) = scanArgs args
          check = elem "--check" flags
          inplace = elem "--inplace" flags
          stdin = elem "--stdin" flags
          config = mkConfigFromOpts opts
          (letStyle, arrowStyle, ifStyle) = mkStylesFromOpts opts
       in Just (MkArgs files config letStyle arrowStyle ifStyle check inplace stdin)

||| Usage string displayed on --help or invalid input.
export showUsage : String
showUsage =
  let helpInfo = H.collectHelpInfo "idris2-fmt" cliParser
  in H.formatHelp helpInfo
