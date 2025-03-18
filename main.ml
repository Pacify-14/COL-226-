(*open Ast

(* parses to AST*)
let parse s = 
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in 
  ast

let string_of_val (e: expr) : string =
  match e with 
  | Int i -> string_of_int i

let interp (s: string) : string = 
  s |> parse |> string_of_val *)

open Ast

(* Parses input to AST and prints it *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* Convert AST to a readable string representation *)
let rec string_of_expr (e: expr) : string =
  match e with
  | Int i -> "Int(" ^ string_of_int i ^ ")"
  | Binop (Add, e1, e2) -> "Binop(+, " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Binop (Sub, e1, e2) -> "Binop(-," ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Binop (Mul, e1, e2) -> "Binop(*," ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Binop (Mod, e1, e2) -> "Binop(%," ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")" 
  | Binop (Div, e1, e2) -> "Binop(/," ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")" 

  | Boolop (And, e1, e2) -> "Boolop(And, " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
| Boolop (Or, e1, e2) -> "Boolop(Or, " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
| Boolop (Not, e1, e2) -> "Boolop(Not, " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
| IfElse (cond, e1, e2) -> "if " ^ string_of_expr cond ^ " then " ^ string_of_expr e1 ^ " else "^ string_of_expr e2
| Var id -> id
| Assign(var , value) -> "Assign(" ^ var ^ " ," ^ string_of_expr value ^ ")"
| Paren e -> "(" ^ string_of_expr e ^ ")"
| Func (f, args) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr args ) ^ ")"
  | _ -> failwith "precondition violated"

let () =
  let input = read_line () in
  let ast = parse input in
  print_endline (string_of_expr ast)


