module Main

import System
import System.File.ReadWrite as SFRW
import System.File.Virtual as SFV
import Data.Either as E
import Data.List as L
import Data.String as S
import Text.PrettyPrint.Bernardy as PP

import IdrisFmt.AST as AST
import IdrisFmt.Config as CFG
import IdrisFmt.Parser as P
import IdrisFmt.Printer as PR
import IdrisFmt.Transform as T
import IdrisFmt.CLI as CLI
import IdrisFmt.Doc as D
import IdrisFmt.Align as A

%default covering

||| Format a single source string.
formatSource : CFG.Config -> String -> Either P.ParseError String
formatSource cfg src =
  map (A.applyAlignment cfg . PR.printModule cfg . T.transformModule cfg) (P.parseModule src)

||| Process a single file: read, format, then write or check.
||| Returns True if the file needs formatting (only meaningful in check mode).
processFile : CFG.Config -> Bool -> Bool -> String -> IO Bool
processFile cfg check inplace file =
  do Right src <- SFRW.readFile file
       | Left err => do putStrLn ("Error reading " ++ file ++ ": " ++ show err)
                        pure False
     case formatSource cfg src of
       Left err     => do putStrLn ("Error formatting " ++ file ++ ": " ++ show err)
                          pure False
       Right output =>
         if check
         then if src == output
              then pure False
              else do putStrLn (file ++ " needs formatting")
                      pure True
         else if inplace
              then do Right () <- SFRW.writeFile file output
                        | Left err => do putStrLn ("Error writing " ++ file ++ ": " ++ show err)
                                         pure False
                      pure False
              else do putStr output
                      pure False

||| Run the formatter with parsed CLI arguments.
export
run : CLI.Args -> IO ()
run args =
  if args.stdin
  then do Right src <- SFRW.fRead SFV.stdin
           | Left err => putStrLn ("Error reading stdin: " ++ show err)
          case formatSource args.config src of
            Left err   => putStrLn ("Error: " ++ show err)
            Right out  => putStr out
  else do needsFmt <- traverse (processFile args.config args.check args.inplace) args.files
          case args.check && any id needsFmt of
            True  => exitFailure
            False => pure ()

covering
main : IO ()
main =
  do args <- getArgs
     case CLI.parseArgs args of
       Nothing     => putStrLn CLI.showUsage
       Just parsed => run parsed
