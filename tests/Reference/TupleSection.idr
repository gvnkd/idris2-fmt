module Reference

foo : Maybe Int -> Maybe (String, Int)
foo mx =
  ("hello",) <$> mx
