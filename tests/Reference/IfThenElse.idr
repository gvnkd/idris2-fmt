module Reference

%default total

guarded : Int -> String
guarded x =
  if x < 0
    then "negative"
    else
      if x == 0 then "zero" else "positive"

guarded' : Int -> String
guarded' x =
  if x < 0
    then "negative"
    else
      if x == 0 then "zero" else "positive"
