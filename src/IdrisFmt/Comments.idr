module IdrisFmt.Comments

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
