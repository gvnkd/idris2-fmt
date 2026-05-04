module Reference

%default total

mutual
  even_ : Nat -> Bool
  even_ Z = True
  even_ (S n) = odd_ n

  odd_ : Nat -> Bool
  odd_ Z = False
  odd_ (S n) = even_ n
