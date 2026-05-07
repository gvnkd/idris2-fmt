module Reference
import Data.List

zip_with_last : List a -> List (a, Maybe a)
zip_with_last list =
  let (_ :: xs) = map Just list
  | [] => []
  in zip list (snoc xs Nothing)
