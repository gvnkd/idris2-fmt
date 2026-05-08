module Reference

record Foo where
  constructor MkFoo
  a : Int
  b : String

test : Foo -> Bool
test foo =case foo of
  MkFoo {} => True
  _ => False
