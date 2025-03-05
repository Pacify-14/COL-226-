let exp = 
  Var of string
| Int of int 
| Mul of exp * exp | Add of exp * exp 
| Let of string * exp * exp 

let rec fv e = 
  match e with 
  | Var x -> [x] (*if x is an extracted variable, then ofcourse it is a variable*)
  | Int _ -> []
  | Add (e1, e2) | Mul (e1, e2) -> fv e1 @ fv e2
  | Let (x, e1, e2) -> 
    let fv_e1 = fv e1 in 
    let fv_e2 = fv e2 in 
    List.filter (fun v -> v <> x) fv_e1 @ fv_e2 (*Remove 'x' from fv_e2 since it is bound in e2...*) (*x is given using eval e1, but then x's value is itself used only inside for calculating the value of e2*)
