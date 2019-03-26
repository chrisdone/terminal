data Terminal a
  = Print String (Terminal a)
  | GetLine (String -> Terminal a)
  | Return a

start :: Terminal ()
start = Return ()
