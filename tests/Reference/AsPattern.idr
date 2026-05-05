module Reference
import Data.List

%default total

asPattern : List a -> Maybe (a, List a)
asPattern xs@(x :: _) =
  Just (x, xs)
asPattern [] =
  Nothing
