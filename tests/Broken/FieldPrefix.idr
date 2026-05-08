module Reference

record Resp where
  task : Maybe String
  issue : Maybe String

getTasks : List Resp -> List (Maybe String)
getTasks rs = map (.task) rs
