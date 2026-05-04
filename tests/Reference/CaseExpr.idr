module Reference

%default total

classify : Int -> String
classify n = case n of
               0 => "zero"
               1 => "one"
               _ => "other"
