module Case

foo : Maybe Int -> Int
foo x = case x of
  Just n => n
  Nothing => 0
