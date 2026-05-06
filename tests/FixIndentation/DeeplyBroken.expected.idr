module DeeplyBroken

foo : IO ()
foo = do
  putStrLn "way too deep"
  putStrLn "should not change"
