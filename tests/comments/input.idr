module Comments

-- A type signature comment
foo : Int -> Int

-- A function comment
foo x = x + 1

{- A block comment
   before bar -}
bar : String -> String
bar s = s ++ "!"
