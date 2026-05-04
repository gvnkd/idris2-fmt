module IdrisFmt.CLI

import System
import Data.List as L
import Data.String as S

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
parseArgs [] = Nothing
parseArgs (_ :: args) = go args CFG.defaultConfig False False False []
  where
    go : List String -> CFG.Config -> Bool -> Bool -> Bool -> List String -> Maybe Args
    go [] cfg check inplace stdin files =
      Just (MkArgs (reverse files) cfg check inplace stdin)
    go ("--check" :: rest) cfg c i s fs =
      go rest cfg True i s fs
    go ("--inplace" :: rest) cfg c i s fs =
      go rest cfg c True s fs
    go ("--stdin" :: rest) cfg c i s fs =
      go rest cfg c i True fs
    go ("--help" :: _) _ _ _ _ _ =
      Nothing
    go ("--indent" :: nStr :: rest) cfg c i s fs =
      case S.parsePositive nStr of
        Nothing => Nothing
        Just n  => go rest (MkConfig n cfg.lineLength cfg.alignRules) c i s fs
    go ("--indent" :: []) _ _ _ _ _ =
      Nothing
    go ("--width" :: nStr :: rest) cfg c i s fs =
      case S.parsePositive nStr of
        Nothing => Nothing
        Just n  => go rest (MkConfig cfg.indentWidth n cfg.alignRules) c i s fs
    go ("--width" :: []) _ _ _ _ _ =
      Nothing
    go (arg :: rest) cfg c i s fs =
      case unpack arg of
        '-' :: '-' :: _ => Nothing
        _               => go rest cfg c i s (arg :: fs)

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
