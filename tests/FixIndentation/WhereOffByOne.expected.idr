module WhereOffByOne

foo : Int -> Int
foo x =
  bar x
  where
    bar : Int -> Int bar y = y + 1
