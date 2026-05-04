module IdrisFmt.Config

%default total

||| Which constructs should have aligned columns.
public export
record AlignRules where
  constructor MkAlignRules
  alignCaseArrows     : Bool  -- align => in case alternatives
  alignTypeSigs       : Bool  -- align : in data/interface decls
  alignFunctionDefs   : Bool  -- align = in adjacent function defs
  alignRecordFields   : Bool  -- align : in record fields
  alignListValues     : Bool  -- align values in multi-line lists

export
defaultAlignRules : AlignRules
defaultAlignRules = MkAlignRules True True True True False

||| Central formatter configuration.
public export
record Config where
  constructor MkConfig
  indentWidth : Nat
  lineLength  : Nat
  alignRules  : AlignRules

export
defaultConfig : Config
defaultConfig = MkConfig 2 80 defaultAlignRules
