module Functions

foo : Int -> Int
foo x = x + 1

bar : String -> String
bar s = s ++ "!"

mapVect : (a -> b) -> Vect n a -> Vect n b
mapVect f []        = []
mapVect f (x :: xs) = f x :: mapVect f xs
