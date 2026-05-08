module Reference

readNat : String -> IO (Maybe Nat)
readNat s =
  pure (Just 42)

f : String -> IO (Maybe Nat)
f s = do
  Just n <- readNat s
  | Nothing => pure Nothing
  pure (Just n)
