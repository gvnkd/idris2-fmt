module Reference

%default total

infixl 5 <+>

sectionL : List Int -> List Int
sectionL xs = map (+ 1) xs
