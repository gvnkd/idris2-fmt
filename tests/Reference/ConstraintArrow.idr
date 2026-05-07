module Reference

%default total

interface ToJSON a where
  encode : a -> String

record CmdResult where
  constructor MkCmdResult
  payload : String

cmdOk : ToJSON a => (text : String) -> (payload : a) -> CmdResult
cmdOk txt val =
  MkCmdResult (encode val)

foo : {auto x : Nat} -> String -> String
foo y =
  y
