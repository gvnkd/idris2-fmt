module Reference

readNat : String -> Maybe Nat
readNat s =
  Just 42

f : String -> IO Nat
f s = do
  let Just n = readNat s
  | Nothing => pure 0
  pure n
