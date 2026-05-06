module IdrisFmt.ConfigFile
import Data.List as L
import Data.List1 as L1
import Data.String as S
import IdrisFmt.Config as CFG
import IdrisFmt.Printer.Complete as PRM
import System.Directory as SD
import System.File.ReadWrite as SFRW
import System.File.Meta as SFM

%default covering

||| Config values that can be set in a config file.
public export
record FileConfig where
  constructor MkFileConfig
  indentWidth : Maybe Nat
  lineLength : Maybe Nat
  letStyle : Maybe PRM.LetStyle
  arrowStyle : Maybe PRM.ArrowStyle
  ifStyle : Maybe PRM.IfStyle
  alignCaseArrows : Maybe Bool
  alignTypeSigs : Maybe Bool
  alignFunctionDefs : Maybe Bool
  alignRecordFields : Maybe Bool

||| Empty file config.
export
emptyFileConfig : FileConfig
emptyFileConfig = MkFileConfig Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

||| Parse a style string.
parseLetStyle : String -> Maybe PRM.LetStyle
parseLetStyle s =
  case toLower s of
    "inline" => Just PRM.Inline
    "auto"   => Just PRM.Auto
    "block"  => Just PRM.Block
    _        => Nothing

parseArrowStyle : String -> Maybe PRM.ArrowStyle
parseArrowStyle s =
  case toLower s of
    "trailing" => Just PRM.Trailing
    "leading"  => Just PRM.Leading
    _          => Nothing

parseIfStyle : String -> Maybe PRM.IfStyle
parseIfStyle s =
  case toLower s of
    "compact"  => Just PRM.Compact
    "indented" => Just PRM.Indented
    _          => Nothing

parseBool : String -> Maybe Bool
parseBool s =
  case toLower s of
    "true"  => Just True
    "false" => Just False
    _       => Nothing

||| Trim whitespace from both ends.
myTrim : String -> String
myTrim s =
  let cs = unpack s
      droppedStart = dropWhile isSpace cs
      droppedEnd = reverse (dropWhile isSpace (reverse droppedStart))
   in pack droppedEnd
  where
    isSpace : Char -> Bool
    isSpace ' ' = True
    isSpace '\t' = True
    isSpace '\n' = True
    isSpace '\r' = True
    isSpace _ = False

||| Parse a single key-value line.
parseLine : String -> Maybe (String, String)
parseLine line =
  let trimmed = myTrim line
   in if null (unpack trimmed) || isPrefixOf (unpack "#") (unpack trimmed)
        then Nothing
        else case break (== '=') (unpack trimmed) of
               (keyChars, '=' :: valChars) =>
                 Just (myTrim (pack keyChars), myTrim (pack valChars))
               _ => Nothing

||| Parse file config from string.
parseFileConfig : String -> FileConfig
parseFileConfig src =
  let lines = filter (not . null . unpack) (lines src)
      kvs = mapMaybe parseLine lines
      lookupKey : String -> Maybe String
      lookupKey key = lookup key kvs
   in MkFileConfig
        (lookupKey "indent" >>= S.parsePositive >>= \i => Just (fromInteger i))
        (lookupKey "width" >>= S.parsePositive >>= \i => Just (fromInteger i))
        (lookupKey "let-style" >>= parseLetStyle)
        (lookupKey "arrow-style" >>= parseArrowStyle)
        (lookupKey "if-style" >>= parseIfStyle)
        (lookupKey "align-case-arrows" >>= parseBool)
        (lookupKey "align-type-sigs" >>= parseBool)
        (lookupKey "align-function-defs" >>= parseBool)
        (lookupKey "align-record-fields" >>= parseBool)

||| Join path components with /.
joinPath : String -> String -> String
joinPath a b =
  if a == "."
    then b
    else if S.isSuffixOf "/" a
      then a ++ b
      else a ++ "/" ++ b

||| Get parent directory.
parentDir : String -> Maybe String
parentDir "/" = Nothing
parentDir path =
  let partsList = forget (S.split (== '/') path)
      parts = filter (not . null . unpack) partsList
   in case reverse parts of
        [] => Nothing
        _ :: [] => Just "/"
        _ :: rest => Just (S.joinBy "/" (reverse rest))

||| Search for .idris2-fmt starting from current dir and walking up.
findConfigFile : String -> IO (Maybe String)
findConfigFile startDir = do
  let configName = ".idris2-fmt"
      candidate = joinPath startDir configName
  exists <- SFM.exists candidate
  if exists
    then pure (Just candidate)
    else case parentDir startDir of
           Nothing => pure Nothing
           Just parent =>
             if parent == startDir
               then pure Nothing
               else findConfigFile parent

||| Read file config if present.
export
loadFileConfig : IO FileConfig
loadFileConfig = do
  cwdRes <- SD.currentDir
  case cwdRes of
    Nothing => pure emptyFileConfig
    Just cwd => do
      mPath <- findConfigFile cwd
      case mPath of
        Nothing => pure emptyFileConfig
        Just path => do
          result <- SFRW.readFile path
          case result of
            Left _ => pure emptyFileConfig
            Right content => pure (parseFileConfig content)

||| Merge file config with base config. File config fills in missing values.
export
mergeBaseConfig : FileConfig -> CFG.Config -> CFG.Config
mergeBaseConfig fc cfg =
  let i = case fc.indentWidth of
            Nothing => cfg.indentWidth
            Just n => n
      w = case fc.lineLength of
            Nothing => cfg.lineLength
            Just n => n
      rules = cfg.alignRules
      newRules = CFG.MkAlignRules
        (case fc.alignCaseArrows of
           Nothing => rules.alignCaseArrows
           Just b => b)
        (case fc.alignTypeSigs of
           Nothing => rules.alignTypeSigs
           Just b => b)
        (case fc.alignFunctionDefs of
           Nothing => rules.alignFunctionDefs
           Just b => b)
        (case fc.alignRecordFields of
           Nothing => rules.alignRecordFields
           Just b => b)
   in CFG.MkConfig i w newRules

||| Extract styles from file config.
export
mergeStyles : FileConfig -> (PRM.LetStyle, PRM.ArrowStyle, PRM.IfStyle) -> (PRM.LetStyle, PRM.ArrowStyle, PRM.IfStyle)
mergeStyles fc (ls, as, is) =
  ( case fc.letStyle of
      Nothing => ls
      Just s => s
  , case fc.arrowStyle of
      Nothing => as
      Just s => s
  , case fc.ifStyle of
      Nothing => is
      Just s => s
  )