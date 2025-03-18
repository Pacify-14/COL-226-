type bop = 
  | Add
  | Sub
  | Mul
  | Div
  | Mod

type boolop = 
  And 
  | Or
  | Not

type expr =
  | Int of int 
  | Binop of bop * expr * expr 
  | Inp of string
  | Print of string 
  | Assign of string * expr      
  | Bool of bool
  | Boolop of boolop * expr * expr
  | IfElse of expr * expr * expr 
  | Var of string
