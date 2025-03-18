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
 
  | _ -> failwith "precondition violated"

let () =
  let input = read_line () in
  let ast = parse input in
  print_endline (string_of_expr ast)


