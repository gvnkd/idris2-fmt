module NestedBlocks

foo : IO ()
foo = do
  let x = 1
  bar x
  where
    bar : Int
            -> IO () bar y =
                 do
                   putStrLn (show y) pure ()
