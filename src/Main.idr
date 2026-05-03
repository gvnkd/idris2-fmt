module Main

import System
import System.File as SF
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

%default total

||| Format a single source string.
||| The pipeline is fused:
|||   parseModule >=> transformModule >=> printModule >=> render
formatSource : CFG.Config -> String -> Either P.ParseError String
formatSource cfg src =
  let opts = D.toLayoutOpts cfg
   in map (D.renderDoc cfg . PR.printModule cfg . T.transformModule cfg)
      (P.parseModule src)

||| Process a single file: read, format, then write or check.
processFile : CFG.Config -> Bool -> String -> IO ()
processFile cfg inplace file = ?rhs_processFile

||| Run the formatter with parsed CLI arguments.
export
run : CLI.Args -> IO ()
run args = ?rhs_run

covering
main : IO ()
main =
  do args <- getArgs
     case CLI.parseArgs args of
       Nothing     => putStrLn CLI.showUsage
       Just parsed => run parsed
