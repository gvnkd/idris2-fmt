module Main

import System
import System.File.ReadWrite as SFRW
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

%default covering

||| Format a single source string.
formatSource : CFG.Config -> String -> Either P.ParseError String
formatSource cfg src =
  map (PR.printModule cfg . T.transformModule cfg) (P.parseModule src)

||| Process a single file: read, format, then write or check.
processFile : CFG.Config -> Bool -> String -> IO ()
processFile cfg inplace file =
  do Right src <- SFRW.readFile file
       | Left err => putStrLn ("Error reading " ++ file ++ ": " ++ show err)
     case formatSource cfg src of
       Left err     => putStrLn ("Error formatting " ++ file ++ ": " ++ show err)
       Right output =>
         if inplace
         then do Right () <- SFRW.writeFile file output
                   | Left err => putStrLn ("Error writing " ++ file ++ ": " ++ show err)
                 pure ()
         else putStrLn output

||| Run the formatter with parsed CLI arguments.
export
run : CLI.Args -> IO ()
run args =
  if args.stdin
  then putStrLn "--stdin not yet implemented"
  else traverse_ (processFile args.config args.inplace) args.files

covering
main : IO ()
main =
  do args <- getArgs
     case CLI.parseArgs args of
       Nothing     => putStrLn CLI.showUsage
       Just parsed => run parsed
