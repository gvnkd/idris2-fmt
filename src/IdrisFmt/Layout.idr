module IdrisFmt.Layout
import Data.List
import Text.PrettyPrint.Bernardy as PP
import Text.PrettyPrint.Bernardy.Combinators

%default total

||| Horizontal-or-vertical: try `x <++> y`, fall back to `x` above `y` indented.
export hov : {opts : _} -> Nat -> Doc opts -> Doc opts -> Doc opts
hov k x y = (x <++> y) <|> (x `vappend` indent k y)

||| Like `hov` but uses `<+>` (no space) for horizontal.
export hov' : {opts : _} -> Nat -> Doc opts -> Doc opts -> Doc opts
hov' k x y = (x <+> y) <|> (x `vappend` indent k y)

||| Horizontal-or-vertical list.
||| All elements on one line, or each on its own line indented.
export spread : {opts : _} -> Nat -> List (Doc opts) -> Doc opts
spread _ [] = empty
spread _ [x] = x
spread k (x :: xs) = foldl (hov k) x xs

||| Horizontal-or-vertical with commas.
export commaSep : {opts : _} -> Nat -> List (Doc opts) -> Doc opts
commaSep _ [] = empty
commaSep _ [x] = x
commaSep k xs =
  let horiz = hsep (intersperse (text ", ") xs)
    in let vert = vsep (map (indent k) (intersperse (text ",") xs))
         in horiz <|> vert

||| `hangSep` helper: keyword + body that breaks.
export hangSep'' : {opts : _} -> Nat -> Doc opts -> Doc opts -> Doc opts
hangSep'' k x y = hangSep k x y

||| Same but with space between keyword and body.
export hangSep''' : {opts : _} -> Nat -> Doc opts -> Doc opts -> Doc opts
hangSep''' k x y = hangSep' k x y

||| Surround with parens, breaking inside if needed.
export parens' : {opts : _} -> Doc opts -> Doc opts
parens' d = hov' 0 lparen (d <+> rparen)

||| Surround with braces, breaking inside if needed.
export braces' : {opts : _} -> Doc opts -> Doc opts
braces' d = hov' 0 lbrace (d <+> rbrace)
