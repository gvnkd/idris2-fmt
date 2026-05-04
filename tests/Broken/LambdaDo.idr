module Reference
import Data.List

%default total

lam : List Int -> List Int
lam xs = map (\x => x * 2) xs

doBlock : IO ()
doBlock = do
  putStrLn "hello"
  x <- getLine
  let y = x ++ "!"
  putStrLn y
