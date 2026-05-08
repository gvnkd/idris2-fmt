module Reference

infixl 3 <*>

(<*>) : String -> String -> String
(<*>) s1 s2 =
  s1 ++ s2
