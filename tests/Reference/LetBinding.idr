module Reference

%default total

letTest : Int -> Int
letTest x = let y = x + 1
            in y * 2
