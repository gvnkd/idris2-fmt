module IdrisFmt.Comments

import Data.List
import Data.String

%default total

||| Style of source comment.
public export
data CommentStyle = LineComment | BlockComment

||| A single source comment with positional metadata.
public export
record Comment where
  constructor MkComment
  style   : CommentStyle
  content : String
  line    : Nat
  col     : Nat

||| Convert a doc string (||| content) into a list of line comments.
export
docToComments : String -> List Comment
docToComments s =
  let lines_ = lines s
      nonEmpty = filter (\l => length l > 0) (map trim lines_)
   in map (\l => MkComment LineComment l 0 0) nonEmpty
