(* typechecker.ml *)

open Ast

exception TypeError of string

(* Checks whether a type is "basic". Since Int and Float now require an argument,
   we match them with a wildcard. *)
let is_basic_type t =
  match t with
  | Int _ | Float _ -> true
  | _ -> false

(* A recursive function to determine the type of an expression.
   Note: The dummy argument "" is supplied to Int and Float.
   Adjust this as needed based on your AST definition. *)
let rec type_of_expr expr =
  match expr with
  | Int _ -> Int ""     (* Assuming Int expects one argument, here provided as an empty string *)
  | Float _ -> Float "" (* Same for Float *)
  |  (Add,e1, e2) ->
      let t1 = type_of_expr e1 in
      let t2 = type_of_expr e2 in
      (* Here we check if both types are basic integers.
         Adjust the equality check as appropriate for your AST. *)
      if t1 = (Int "") && t2 = (Int "") then Int ""
      else raise (TypeError "Addition requires two integers")
  | Sub (e1, e2) ->
      let t1 = type_of_expr e1 in
      let t2 = type_of_expr e2 in
      if t1 = (Int "") && t2 = (Int "") then Int ""
      else raise (TypeError "Subtraction requires two integers")
  | Mul (e1, e2) ->
      let t1 = type_of_expr e1 in
      let t2 = type_of_expr e2 in
      if t1 = (Int "") && t2 = (Int "") then Int ""
      else raise (TypeError "Multiplication requires two integers")
  | Div (e1, e2) ->
      let t1 = type_of_expr e1 in
      let t2 = type_of_expr e2 in
      if t1 = (Int "") && t2 = (Int "") then Int ""
      else raise (TypeError "Division requires two integers")
  | _ -> raise (TypeError "Unsupported expression")

(* Check the type of an expression and return its type if it is basic.
   Uses the string_of_type function (assumed to be defined in Ast) to produce an error message. *)
let check_expr expr =
  let t = type_of_expr expr in
  if is_basic_type t then
    t
  else
    raise (TypeError ("Expression has non-basic type: " ^ string_of_type t))

