module Reference

record Parser a where
  constructor MkParser
  runParser : String -> a
