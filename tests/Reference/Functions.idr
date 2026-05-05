module Reference
import Data.List

%default total

foo : Int -> Int
foo x =
  x + 1

bar : Int -> String -> Bool
bar n s =
  n > 0 && length s > 0

baz : Int -> Int
baz x =
  x * 2

pat : List a -> Nat
pat [] =
  0
pat (_ :: xs) =
  1 + pat xs
