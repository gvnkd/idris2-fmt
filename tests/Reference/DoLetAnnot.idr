module Reference
import Data.List

foo : IO ()
foo = do
  let getters : List (String, Int) = [("task", 1)]
  printLn (length getters)
