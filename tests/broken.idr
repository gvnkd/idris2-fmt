module Reference
import Data.List
import Data.Maybe
import public Data.String

%default total

||| A data type
data Foo : Type where
  MkFoo : Int -> Foo
  MkFooBar : Int -> Foo

data Vect : Nat -> Type -> Type where
  Nil : Vect 0 a
  (::) : a -> Vect n a -> Vect (S n) a
-- A type signature comment
foo : Int -> Int
-- A function comment
foo x = x + 1
{- A block comment
   before bar -}
bar : Int -> String -> Bool
bar n s = n > 0 && length s > 0

baz : Int -> Int
baz x = x * 2

pat : List a -> Nat
pat [] = 0
pat (_ :: xs) = 1 + pat xs

guarded : Int -> String
guarded x = if x < 0 then "negative" else if x == 0 then "zero" else "positive"

lam : List Int -> List Int
lam xs = map (\x => x * 2) xs

doBlock : IO ()
doBlock = do
putStrLn "hello"
x <- getLine
let y = x ++ "!"
putStrLn y

data Tree : Type -> Type where
  Leaf : Tree a
  Node : a -> Tree a -> Tree a -> Tree a

||| A record
record Point where
  constructor MkPoint
  x : Double
  y : Double

interface Showable a where
  showIt : a -> String

export
implementation Showable Int where
  showIt n = show n

mutual
  even_ : Nat -> Bool
  even_ Z = True
  even_ (S n) = odd_ n
  odd_ : Nat -> Bool
  odd_ Z = False
  odd_ (S n) = even_ n

neg : Int -> Int
neg x = -x

section : List Int -> List Int
section xs = filter (> 0) xs

tuple : (Int, String)
tuple = (1, "hello")

listLit : List Int
listLit = [1, 2, 3]

interp : String -> String
interp name = "Hello, \{name}!"

holeFn : Int -> Int
holeFn x = ?rhs_holeFn

eta : Int -> Int
eta = (+ 1)

charLit : Char
charLit = 'a'

impl : {a : Type} -> a -> a
impl x = x

namedApp : List Int
namedApp = replicate 3 0

bracket : Int
bracket = (1 + 2) * 3

testTuple : (Int, Int) -> Int
testTuple (x, y) = x

testTuple' : (Int, Int) -> Int
testTuple' = snd
