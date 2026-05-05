module Reference

%default total

interface Showable a where
  showIt : a -> String

export
implementation Showable Int where
  showIt n = show n
