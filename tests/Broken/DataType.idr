module Reference
import Data.List
import Data.Maybe

%default total

data Foo : Type where
                MkFoo : Int -> Foo
                MkFooBar : Int -> Foo

data Vect : Nat -> Type -> Type where
                Nil : Vect 0 a
                (::) : a -> Vect n a -> Vect (S n) a
