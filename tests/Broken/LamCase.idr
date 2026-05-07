module Reference

foo : String -> Maybe String
foo =
  \case
    "ok" => Just "ok"
    _ => Nothing
