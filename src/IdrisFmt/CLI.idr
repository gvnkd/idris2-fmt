module IdrisFmt.CLI

import System
import Data.List as L

import IdrisFmt.Config as CFG

%default total

||| Parsed command-line arguments.
public export
record Args where
  constructor MkArgs
  files   : List String
  config  : CFG.Config
  check   : Bool
  inplace : Bool
  stdin   : Bool

||| Parse raw command-line arguments into structured Args.
export
parseArgs : List String -> Maybe Args
parseArgs args = ?rhs_parseArgs

||| Usage string displayed on --help or invalid input.
export
showUsage : String
showUsage = """
idris2-fmt [options] <files...>

Options:
  --check       Check formatting without writing
  --inplace     Edit files in place
  --stdin       Read from stdin
  --indent N    Indentation width (default: 2)
  --width N     Line length (default: 80)
  --help        Show this help
"""
