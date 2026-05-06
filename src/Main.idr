module Main
import Data.List as L
import Data.String as S
import IdrisFmt.AST as AST
import IdrisFmt.Align as A
import IdrisFmt.CLI as CLI
import IdrisFmt.Config as CFG
import IdrisFmt.ConfigFile as CF
import IdrisFmt.Doc as D
import IdrisFmt.Parser as P
import IdrisFmt.Printer as PR
import IdrisFmt.Printer.Complete as PRM
import IdrisFmt.Transform as T
import System
import System.File.ReadWrite as SFRW
import System.File.Virtual as SFV

%default covering

||| Convert base config and style options to formatter config.
mkFmtConfig : CFG.Config -> PRM.LetStyle -> PRM.ArrowStyle -> PRM.IfStyle -> PRM.FmtConfig
mkFmtConfig cfg ls as is = PRM.MkFmtConfig cfg ls as is

||| Format a single source string.
formatSource : CFG.Config -> PRM.LetStyle -> PRM.ArrowStyle -> PRM.IfStyle -> String -> Either P.ParseError String
formatSource cfg ls as is src =
  map (PRM.printModuleM (mkFmtConfig cfg ls as is) . T.transformModule cfg)
    (P.parseModule src)

||| Process a single file: read, format, then write or check.
||| Returns True if the file needs formatting (only meaningful in check mode).
processFile : CFG.Config -> PRM.LetStyle -> PRM.ArrowStyle -> PRM.IfStyle -> Bool -> Bool -> String -> IO Bool
processFile cfg ls as is check inplace file = do
  srcResult <-
    SFRW.readFile file
  case srcResult of
    Left err => do
      putStrLn ("Error reading " ++ file ++ ": " ++ show err)
      pure False
    Right src =>
      case formatSource cfg ls as is src of
        Left err => do
          putStrLn ("Error formatting " ++ file ++ ": " ++ show err)
          pure False
        Right output =>
          if check
            then
              if src == output
                then pure False
                else do
                  putStrLn (file ++ " needs formatting")
                  pure True
            else
              if inplace
                then do
                  writeResult <-
                    SFRW.writeFile file output
                  case writeResult of
                    Left err => do
                      putStrLn ("Error writing " ++ file ++ ": " ++ show err)
                      pure False
                    Right () =>
                      pure False
                else do
                  putStr output
                  pure False

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
  if args.stdin
    then do
      srcResult <-
        SFRW.fRead SFV.stdin
      case srcResult of
        Left err =>
          putStrLn ("Error reading stdin: " ++ show err)
        Right src =>
          case formatSource args.config args.letStyle args.arrowStyle args.ifStyle src of
            Left err =>
              putStrLn ("Error: " ++ show err)
            Right out =>
              putStr out
    else do
      needsFmt <-
        traverse (processFile args.config args.letStyle args.arrowStyle args.ifStyle args.check args.inplace) args.files
      case args.check && any id needsFmt of
        True =>
          exitFailure
        False =>
          pure ()

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
