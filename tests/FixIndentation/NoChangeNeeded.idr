module NoChangeNeeded

foo : IO ()
foo = do
  putStrLn "already correct"
  putStrLn "no fix needed"
