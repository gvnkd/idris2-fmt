module IdrisFmt.Comments
import Data.List
import Data.String

%default total

||| Style of source comment.
public export
data CommentStyle : Type where
  LineComment : CommentStyle
  BlockComment : CommentStyle
  DocComment : CommentStyle

||| A single source comment with positional metadata.
public export
record Comment where
  constructor MkComment
  style : CommentStyle
  content : String
  line : Nat
  col : Nat

||| Convert a doc string (||| content) into a list of doc comments.
export docToComments : String -> List Comment
docToComments s =
  let lines_ = lines s
    in let nonEmpty = filter (\l => length l > 0) (map trim lines_)
         in map (\l => MkComment DocComment l 0 0) nonEmpty
