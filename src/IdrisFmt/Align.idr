module IdrisFmt.Align
import Data.List as L
import Data.String as S
import IdrisFmt.Config as CFG

%default covering

||| Find the 1-based column of the first occurrence of a substring in a string.
findCol : String -> String -> Maybe Nat
findCol needle haystack = go 1 (unpack haystack)
  where
    go : Nat -> List Char -> Maybe Nat
    go _ [] = Nothing
    go n cs@(_ :: rest) =
      if isPrefixOf (unpack needle) cs then Just n else go (S n) rest

||| Build a string of N spaces.
spaces : Nat -> String
spaces n = pack (replicate n ' ')

||| Maximum of a list of Nats.
maximumNat : List Nat -> Maybe Nat
maximumNat [] = Nothing
maximumNat (x :: xs) = Just (foldl max x xs)

||| Check if a line has a token at the given indentation level (spaces only).
hasTokenAtIndent : Nat -> String -> String -> Bool
hasTokenAtIndent indent token line =
  let leading = length (takeWhile (== ' ') (unpack line))
    in leading == indent && case findCol token line of
                              Nothing => False
                              Just col => col >= S indent

||| Pad spaces after the first word to push token to target column.
alignLine : String -> String -> Nat -> String
alignLine token line targetCol =
  case findCol token line of
    Nothing => line
    Just col =>
      if col >= targetCol
        then line
        else let pad = targetCol `minus` col
               in let n = col `minus` 1
                    in let (before, after) = strSplitAt n line
                         in before ++ spaces pad ++ after
  where
    strSplitAt : Nat -> String -> (String, String)
    strSplitAt n s = let bs = take n (unpack s)
                       in let as = drop n (unpack s) in (pack bs, pack as)

||| Align a single block of lines on the given token.
alignBlock : String -> List String -> List String
alignBlock token lines =
  let cols = mapMaybe (findCol token) lines
    in case maximumNat cols of
         Nothing => lines
         Just targetCol => map (\l => alignLine token l targetCol) lines

||| Group consecutive lines that contain the token at the same indentation.
groupBlocks : Nat -> String -> List String -> List (List String)
groupBlocks _ _ [] = []
groupBlocks minIndent token (l :: ls) =
  let indent = length (takeWhile (== ' ') (unpack l))
    in if indent >= minIndent
         then case findCol token l of
                Nothing => groupBlocks minIndent token ls
                Just col =>
                  if col >= S indent
                    then let (block, rest) = span
                                               (hasTokenAtIndent indent token)
                                               (l :: ls)
                           in block :: groupBlocks minIndent token rest
                    else groupBlocks minIndent token ls
         else groupBlocks minIndent token ls

||| Apply alignment for one token type.
alignToken : Nat -> String -> String -> String
alignToken minIndent token src =
  let lines_ = lines src
    in let blocks = groupBlocks minIndent token lines_
         in let aligned = map (alignBlock token) blocks
              in let merged = mergeBlocks lines_ aligned in unlines merged
  where
    mergeBlocks : List String -> List (List String) -> List String
    mergeBlocks [] _ = []
    mergeBlocks xs [] = xs
    mergeBlocks (x :: xs) (b :: bs) =
      case b of
        (y :: _) =>
          if x == y
            then b ++ mergeBlocks (drop (length b) (x :: xs)) bs
            else x :: mergeBlocks xs (b :: bs)
        [] => x :: mergeBlocks xs (b :: bs)
    mergeBlocks xs _ = xs

||| Post-process rendered output to apply alignment rules.
export applyAlignment : CFG.Config -> String -> String
applyAlignment cfg src =
  let rules = cfg.alignRules
    in applySteps rules.alignCaseArrows
         " => " $ applySteps rules.alignTypeSigs
                    " : " $ applySteps rules.alignFunctionDefs
                              " = " $ applySteps rules.alignRecordFields " : "
                                        src
  where
    applySteps : Bool -> String -> String -> String
    applySteps True tok s = alignToken cfg.indentWidth tok s
    applySteps False _ s = s
