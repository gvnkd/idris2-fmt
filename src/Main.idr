module Main
import Data.List as L
import Data.String as S
import IdrisFmt.AST as AST
import IdrisFmt.Align as A
import IdrisFmt.CLI as CLI
import IdrisFmt.Config as CFG
import IdrisFmt.ConfigFile as CF
import IdrisFmt.Doc as D
import IdrisFmt.IndentFix as IF
import IdrisFmt.Parser as P
import IdrisFmt.Printer as PR
import IdrisFmt.Printer.Complete as PRM
import IdrisFmt.Transform as T
import System
import System.File.ReadWrite as SFRW
import System.File.Virtual as SFV

%default covering

||| Formatter version.
versionString : String
versionString = "0.1.0"

||| Convert base config and style options to formatter config.
mkFmtConfig : CFG.Config -> PRM.LetStyle -> PRM.ArrowStyle -> PRM.IfStyle -> PRM.FmtConfig
mkFmtConfig cfg ls as is = PRM.MkFmtConfig cfg ls as is

||| Format a single source string.
formatSource : CFG.Config -> PRM.LetStyle -> PRM.ArrowStyle -> PRM.IfStyle -> String -> Either P.ParseError String
formatSource cfg ls as is src =
  map (PRM.printModuleM (mkFmtConfig cfg ls as is) . T.transformModule cfg)
    (P.parseModule src)

||| Try to format, optionally fixing indentation first.
formatSourceWithFix : CFG.Config -> PRM.LetStyle -> PRM.ArrowStyle -> PRM.IfStyle -> Bool -> String -> Either P.ParseError String
formatSourceWithFix cfg ls as is fixSrc src =
  case formatSource cfg ls as is src of
    Right result => Right result
    Left err =>
      if fixSrc
        then formatSource cfg ls as is (IF.fixIndentation src)
        else Left err

||| Print error to stderr.
printErr : String -> IO ()
printErr msg = do
  let stderr = SFV.stderr
  _ <- SFRW.fPutStrLn stderr msg
  pure ()

||| Process a single file: read, format, then write or check.
||| Returns Just True if file needs formatting, Just False if ok, Nothing on error.
processFile : CFG.Config -> PRM.LetStyle -> PRM.ArrowStyle -> PRM.IfStyle -> Bool -> Bool -> Bool -> String -> IO (Maybe Bool)
processFile cfg ls as is fixSrc check inplace file = do
  srcResult <-
    SFRW.readFile file
  case srcResult of
    Left err => do
      printErr ("Error reading " ++ file ++ ": " ++ show err)
      pure Nothing
    Right src =>
      case formatSourceWithFix cfg ls as is fixSrc src of
        Left err => do
          printErr ("Error formatting " ++ file ++ ": " ++ show err)
          pure Nothing
        Right output =>
          if check
            then
              if src == output
                then pure (Just False)
                else do
                  putStrLn (file ++ " needs formatting")
                  pure (Just True)
            else
              if inplace
                then do
                  writeResult <-
                    SFRW.writeFile file output
                  case writeResult of
                    Left err => do
                      printErr ("Error writing " ++ file ++ ": " ++ show err)
                      pure (Just False)
                    Right () =>
                      pure (Just False)
                else do
                  putStr output
                  pure (Just False)

||| Merge CLI args with file config. CLI args take precedence.
mergeArgsWithFileConfig : CF.FileConfig -> CLI.Args -> CLI.Args
mergeArgsWithFileConfig fc args =
  let mergedConfig = CF.mergeBaseConfig fc args.config
      mergedStyles = CF.mergeStyles fc (args.letStyle, args.arrowStyle, args.ifStyle)
   in record { config = mergedConfig
             , letStyle = fst3 mergedStyles
             , arrowStyle = snd3 mergedStyles
             , ifStyle = thd3 mergedStyles
             } args
  where
    fst3 : (a, b, c) -> a
    fst3 (x, _, _) = x
    snd3 : (a, b, c) -> b
    snd3 (_, y, _) = y
    thd3 : (a, b, c) -> c
    thd3 (_, _, z) = z

||| Run the formatter with parsed CLI arguments.
export run : CLI.Args -> IO ()
run args =
  if args.version
    then
      putStrLn versionString
    else if args.stdin
      then do
        srcResult <-
          SFRW.fRead SFV.stdin
        case srcResult of
          Left err =>
            do
              printErr ("Error reading stdin: " ++ show err)
              exitFailure
          Right src =>
            case formatSourceWithFix args.config args.letStyle args.arrowStyle args.ifStyle args.fixIndentation src of
              Left err =>
                do
                  printErr ("Error: " ++ show err)
                  exitFailure
              Right out =>
                putStr out
      else do
        results <-
          traverse (processFile args.config args.letStyle args.arrowStyle args.ifStyle args.fixIndentation args.check args.inplace) args.files
        let hasErrors = any isNothing results
            needsFmt = any (== Just True) results
        if hasErrors
          then exitFailure
          else if args.check && needsFmt
            then exitFailure
            else pure ()

%noinline main : IO ()
main = do
  args <-
    getArgs
  fileConfig <- CF.loadFileConfig
  case CLI.parseArgs args of
    Nothing =>
      putStrLn CLI.showUsage
    Just parsed =>
      let merged = mergeArgsWithFileConfig fileConfig parsed
       in run merged