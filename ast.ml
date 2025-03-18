type bop = 
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Lt | Gt | Le | Ge | Ne | Eq

type boolop = 
  And 
  | Or
  | Not

type expr =
  | Int of int 
  | Float of float
  | String of string
  | Binop of bop * expr * expr 
  | Inp of string
  | Print of string 
  | Assign of string * expr      
  | Bool of bool
  | Boolop of boolop * expr * expr
  | IfElse of expr * expr * expr 
  | Var of string
  | Func of string * expr list
  | Paren of expr
  | Block of expr list 
