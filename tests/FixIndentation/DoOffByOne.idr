module DoOffByOne

foo : IO ()
foo = do
  putStrLn "first"
   putStrLn "second"
  putStrLn "third"
