module IdrisFmt.Measure
import Data.List
import Data.String
import Text.PrettyPrint.Bernardy.Combinators as Combinators
import Text.PrettyPrint.Bernardy.Core as Core

%default total

||| Measure the width of a Doc by rendering it and taking the first line.
||| For simple single-line docs this gives the visual width.
export measureWidth : {opts : _} -> Doc opts -> Nat
measureWidth doc =
  let
    rendered = Core.Doc.render opts doc
    ls       = lines rendered
  in case ls of
       [] =>
         0
       (l :: _) =>
         length l

||| Maximum of a list of Nats. Returns 0 for empty list.
export maxWidth : List Nat -> Nat
maxWidth [] =
  0
maxWidth (x :: xs) =
  foldl max x xs

||| Pad a Doc with trailing spaces to reach a target width.
||| Used for alignment: pad the binder/pattern so that `=` / `=>` lands
||| at the same column for all bindings in a block.
export padTo : {opts : _} -> Nat -> Doc opts -> Doc opts
padTo target doc =
  let w = measureWidth doc
    in if w >= target
         then doc
         else doc <+> text (pack (replicate (target `minus` w) ' '))
