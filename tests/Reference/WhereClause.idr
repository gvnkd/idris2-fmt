module Reference

%default total

whereTest : Nat -> Nat
whereTest n =
  square + cube
  where
    square : Nat
    square =
      n * n
    cube : Nat
    cube =
      square * n

whereTest' : Nat -> Nat
whereTest' n =
  square + cube
  where
    square : Nat
    square =
      n * n
    cube : Nat
    cube =
      square * n
