module CaseOffByOne

foo : Maybe Int -> Int
foo x = case x of
  Just n => n + 1
   Nothing => 0
