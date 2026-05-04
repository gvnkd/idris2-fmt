module Reference
import Data.List

%default total

data Tree : Type -> Type where
  Leaf : Tree a
  Node : a -> Tree a -> Tree a -> Tree a
