module Reference
import Data.List

%default total
-- Dependent pair in expression
foo : (n ** Nat) -> Nat
foo p =
  case p of
    (n ** m) => n + m
-- Dependent pair in lambda
bar : Nat
bar =
  let p = (1 ** 2) in case p of
                        (n ** m) => n + m
-- Dependent pair with explicit type
baz : (n : Nat ** Nat) -> Nat
baz p =
  case p of
    (n ** m) => n + m
