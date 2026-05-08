module Reference

%foreign "C:isatty,libc"
prim__isatty : Int -> PrimIO Int
