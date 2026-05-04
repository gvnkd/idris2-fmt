module IdrisFmt.Config

%default total

||| Which constructs should have aligned columns.
public export record AlignRules where
                constructor MkAlignRules
                alignCaseArrows : Bool
                alignTypeSigs : Bool
                alignFunctionDefs : Bool
                alignRecordFields : Bool
                alignListValues : Bool

-- align => in case alternatives
-- align : in data/interface decls
-- align = in adjacent function defs
-- align : in record fields
-- align values in multi-line lists

export defaultAlignRules : AlignRules
defaultAlignRules = MkAlignRules True True True True False

||| Central formatter configuration.
public export record Config where
                constructor MkConfig
                indentWidth : Nat
                lineLength : Nat
                alignRules : AlignRules

export defaultConfig : Config
defaultConfig = MkConfig 2 80 defaultAlignRules
