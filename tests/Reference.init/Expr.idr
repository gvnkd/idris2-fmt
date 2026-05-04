module Reference
import Data.List

%default total

neg : Int -> Int
neg x = - x

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

bracket : Int
bracket = (1 + 2) * 3

testTuple : (Int, Int) -> Int
testTuple (x, y) = x

testTuple' : (Int, Int) -> Int
testTuple' = snd
