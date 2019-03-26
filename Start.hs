data Terminal a
  = Print String (Terminal a)
  | GetLine (String -> Terminal a)
  | Return a

start :: Terminal ()
start =
  Print
    "Enter your name"
    (GetLine (\line -> Print ("Hello, " ++ line) (Return ())))
