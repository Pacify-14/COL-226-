type bop = 
  | Add
  | Sub
  | Mul
type expr =
  | Int of int 
  | Binop of bop * expr * expr 
  
      

