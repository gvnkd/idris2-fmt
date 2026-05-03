module IdrisFmt.Config

%default total

||| Central formatter configuration.
public export
record Config where
  constructor MkConfig
  indentWidth : Nat
  lineLength  : Nat

export
defaultConfig : Config
defaultConfig = MkConfig 2 80
