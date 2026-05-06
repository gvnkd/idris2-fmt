module IdrisFmt.IndentFix
import Data.List as L
import Data.String as S

%default covering

||| Get leading whitespace count for a line.
leadingSpaces : String -> Nat
leadingSpaces s =
  length (takeWhile (\c => c == ' ' || c == '\t') (unpack s))

||| Replace leading whitespace with N spaces.
setIndent : Nat -> String -> String
setIndent n line =
  let content = dropWhile (\c => c == ' ' || c == '\t') (unpack line)
   in pack (replicate n ' ' ++ content)

||| Check if a line is empty or whitespace-only.
isBlankLine : String -> Bool
isBlankLine s =
  all (\c => c == ' ' || c == '\t' || c == '\n' || c == '\r') (unpack s)

||| Get non-blank lines before and after index from a list.
prevNextIndent : List String -> Nat -> (Maybe Nat, Maybe Nat)
prevNextIndent lines i =
  let prevLines = reverse (take (fromNat i) lines)
      nextLines = drop (S (fromNat i)) lines
      prevNonBlank = findFirstNonBlank prevLines
      nextNonBlank = findFirstNonBlank nextLines
   in (prevNonBlank, nextNonBlank)
  where
    fromNat : Nat -> Nat
    fromNat n = n

    findFirstNonBlank : List String -> Maybe Nat
    findFirstNonBlank [] = Nothing
    findFirstNonBlank (l :: ls) =
      if isBlankLine l
        then findFirstNonBlank ls
        else Just (leadingSpaces l)

||| Fix a single body line using neighbor context.
fixBodyLineWithContext : List String -> Nat -> String -> String
fixBodyLineWithContext lines i line =
  let (prevIndent, nextIndent) = prevNextIndent lines i
      currentIndent = leadingSpaces line
   in case (prevIndent, nextIndent) of
        (Just p, Just n) =>
          if p == n && currentIndent > p && (currentIndent `minus` p) <= 2
            then setIndent p line
            else line
        _ => line

||| Zip list with indices starting from n.
zipWithIndex : List a -> Nat -> List (Nat, a)
zipWithIndex [] _ = []
zipWithIndex (x :: xs) n = (n, x) :: zipWithIndex xs (S n)

||| Build fixed lines from original + body fixes.
buildFixedLines : List String -> List String -> Nat -> Nat -> List String
buildFixedLines lines bodyLines bodyStart bodyCount =
  take bodyStart lines ++ bodyLines ++ drop (bodyStart + bodyCount) lines

||| Fix body lines using context-aware heuristic.
fixBodyLines : List String -> Nat -> Nat -> List String
fixBodyLines lines bodyStart bodyCount =
  let bodyLines = take bodyCount (drop bodyStart lines)
      indexed = zipWithIndex bodyLines 0
   in map (\(idx, line) => fixBodyLineWithContext lines (bodyStart + idx) line) indexed

||| Fix indentation in a single block starting at line index i.
||| Returns (fixed lines, number of body lines consumed).
fixBlock : List String -> Nat -> Nat -> (List String, Nat)
fixBlock lines i baseIndent =
  let bodyStart = S i
      bodyLines = takeWhile (\l => not (isBlankLine l) && leadingSpaces l > baseIndent) (drop bodyStart lines)
      bodyCount = length bodyLines
   in if bodyCount == 0
        then (lines, 0)
        else
          let fixedBody = fixBodyLines lines bodyStart bodyCount
           in (buildFixedLines lines fixedBody bodyStart bodyCount, bodyCount)

||| Check if a line is a block opener (do, let, where, case ... of).
||| Matches both suffix form (e.g., "foo = do") and prefix form (e.g., "let x = 1").
isBlockOpener : String -> Bool
isBlockOpener line =
  let trimmed = S.trim line
      startsWithDo = S.isPrefixOf "do " (trimmed ++ " ")
      startsWithWhere = S.isPrefixOf "where " (trimmed ++ " ")
      startsWithOf = S.isPrefixOf "of " (trimmed ++ " ")
      startsWithLet = S.isPrefixOf "let " (trimmed ++ " ")
      startsWithIn = S.isPrefixOf "in " (trimmed ++ " ")
   in S.isSuffixOf " do" trimmed ||
      trimmed == "do" ||
      startsWithDo ||
      S.isSuffixOf " where" trimmed ||
      trimmed == "where" ||
      startsWithWhere ||
      S.isSuffixOf " of" trimmed ||
      startsWithOf ||
      S.isSuffixOf " let" trimmed ||
      trimmed == "let" ||
      startsWithLet ||
      S.isSuffixOf " in" trimmed ||
      startsWithIn

||| Get line at index, or empty string if out of bounds.
getLineAt : Nat -> List String -> String
getLineAt _ [] = ""
getLineAt Z (l :: _) = l
getLineAt (S n) (_ :: ls) = getLineAt n ls

||| Check if a line is a bare %language directive missing its argument.
isBareLanguage : String -> Bool
isBareLanguage line =
  let trimmed = S.trim line
      prefixLen = length (unpack "%language ")
      rest = drop prefixLen (unpack trimmed)
   in trimmed == "%language" || (S.isPrefixOf "%language " trimmed && S.trim (pack rest) == "")

||| Fix bare %language by adding ElabReflection.
fixBareLanguage : String -> String
fixBareLanguage line =
  if isBareLanguage line
    then "%language ElabReflection"
    else line

||| Apply bare %language fix to all lines.
fixBareLanguages : List String -> List String
fixBareLanguages = map fixBareLanguage

||| Apply indentation fixes and bare directive fixes to all lines.
export
fixIndentation : String -> String
fixIndentation src =
  let lines_ = S.lines src
      fixedLines = fixAllBlocks lines_ 0
      fixedDirectives = fixBareLanguages fixedLines
   in S.unlines fixedDirectives
  where
    fixAllBlocks : List String -> Nat -> List String
    fixAllBlocks ls i =
      if i >= length ls
        then ls
        else
          let line = getLineAt i ls
           in if isBlockOpener line && not (isBlankLine line)
                then
                  let baseIndent = leadingSpaces line
                      (fixed, consumed) = fixBlock ls i baseIndent
                   in fixAllBlocks fixed (i + S consumed)
                else fixAllBlocks ls (S i)