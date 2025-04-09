(* Lambda expression syntax tree *)
type expr =
  | Var of string
  | App of expr * expr
  | Lam of string * expr

(* ---------- Krivine Machine ---------- *)
module Krivine = struct
  (* Closure for the Krivine machine *)
  type closure = Closure of expr * (string -> closure)
  
  (* Stack of closures *)
  type stack = closure list
  
  (* The Krivine machine state *)
  type state = closure * stack
  
  (* Helper for printing *)
  let rec string_of_expr = function
    | Var s -> s
    | App (e1, e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")"
    | Lam (x, e) -> "(λ" ^ x ^ "." ^ string_of_expr e ^ ")"

  (* Evaluation step according to the rules *)
  let step (cl, stack) =
    match cl with
    | Closure (App (e1, e2), env) ->
        (* (Op) rule *)
        Some (Closure (e1, env), Closure (e2, env) :: stack)
    | Closure (Var x, env) ->
        (* (Var) rule *)
        (try Some (env x, stack)
         with _ -> None) (* Handle free variables *)
    | Closure (Lam (x, e), env) ->
        (* (App) rule *)
        match stack with
        | [] -> None (* No more reduction possible *)
        | cl' :: stack' ->
            let new_env y = if y = x then cl' else env y in
            Some (Closure (e, new_env), stack')

  (* Evaluate until no more steps can be taken *)
  let rec eval state =
    match step state with
    | None -> state
    | Some state' -> eval state'

  (* Create a default environment - returns variable itself for free variables *)
  let default_env x = Closure (Var x, fun y -> failwith ("Nested free variable: " ^ y))

  (* Evaluate a lambda expression *)
  let eval_expr e =
    let initial_state = (Closure (e, default_env), []) in
    eval initial_state

  (* Unload function to convert a closure back to an expression *)
  let rec unload (Closure (e, env)) =
    match e with
    | Var x -> 
        (* Try to unload from environment, or keep as variable *)
        (try 
          let Closure (e', _) = env x in
          match e' with
          | Var y when y = x -> Var x  (* Handle self-reference *)
          | _ -> unload (env x)
        with _ -> Var x)
    | App (e1, e2) ->
        App (unload (Closure (e1, env)), unload (Closure (e2, env)))
    | Lam (x, e1) ->
        (* Create a fresh variable to avoid capture *)
        let fresh_var = x in  (* Simplified; in practice we'd need actual fresh vars *)
        let new_env y = if y = x then Closure (Var fresh_var, default_env) else env y in
        Lam (fresh_var, unload (Closure (e1, new_env)))
end

(* ---------- SECD Machine ---------- *)
module SECD = struct
  (* Opcodes for the SECD machine *)
  type opcode =
    | LOOKUP of string
    | MkCLOS of string * opcode list
    | APP
    | RET

  (* Values in the SECD machine *)
  type value = 
    | Closure of string * opcode list * (string -> value)

  (* SECD machine state *)
  type state = {
    s: value list;      (* Stack *)
    e: string -> value; (* Environment *)
    c: opcode list;     (* Control (opcodes) *)
    d: dump list        (* Dump (saved contexts) *)
  }
  and dump = Dump of value list * (string -> value) * opcode list

  (* Compiler: convert lambda expressions to opcodes *)
  let rec compile = function
    | Var x -> [LOOKUP x]
    | App (e1, e2) -> compile e1 @ compile e2 @ [APP]
    | Lam (x, e) -> [MkCLOS (x, compile e @ [RET])]

  (* Evaluation step *)
  let step state =
    match state with
    | { s; e; c = LOOKUP x :: c'; d } ->
        (* (Var) rule *)
        (try Some { s = e x :: s; e; c = c'; d }
         with _ -> None)  (* Handle free variables gracefully *)
    
    | { s; e; c = MkCLOS (x, c1) :: c'; d } ->
        (* (Clos) rule *)
        Some { s = Closure (x, c1, e) :: s; e; c = c'; d }
    
    | { s = v2 :: Closure (x, c1, e1) :: s'; e; c = APP :: c'; d } ->
        (* (App) rule *)
        let new_env y = if y = x then v2 else e1 y in
        Some { s = []; e = new_env; c = c1; d = Dump (s', e, c') :: d }
    
    | { s = v :: s'; e; c = RET :: _; d = Dump (s_old, e_old, c_old) :: d' } ->
        (* (Ret) rule *)
        Some { s = v :: s_old; e = e_old; c = c_old; d = d' }
    
    | _ -> None (* No more reduction possible *)

  (* Evaluate until no more steps can be taken *)
  let rec eval state =
    match step state with
    | None -> state
    | Some state' -> eval state'

  (* Create a default environment - handles free variables by creating a closure *)
  let default_env x = 
    Closure (x, [], fun y -> failwith ("Free variable in closure: " ^ y))

  (* Evaluate a lambda expression *)
  let eval_expr e =
    let initial_state = { 
      s = []; 
      e = default_env; 
      c = compile e; 
      d = [] 
    } in
    eval initial_state

  (* Helper function to extract result from final state *)
  let get_result state =
    match state.s with
    | [v] -> v
    | _ -> failwith "Unexpected final stack state"

  (* Convert a value to a string representation *)
  let rec string_of_value = function
    | Closure (x, _, _) -> "Closure(" ^ x ^ ", <code>, <env>)"
end

(* ---------- Example Functions and Testing ---------- *)

(* Some common lambda expressions *)
let id = Lam ("x", Var "x")
let apply = Lam ("f", Lam ("x", App (Var "f", Var "x")))
let true_lam = Lam ("x", Lam ("y", Var "x"))
let false_lam = Lam ("x", Lam ("y", Var "y"))
let if_then_else = Lam ("c", Lam ("t", Lam ("f", App (App (Var "c", Var "t"), Var "f"))))

(* Church numerals *)
let church_0 = Lam ("f", Lam ("x", Var "x"))
let church_1 = Lam ("f", Lam ("x", App (Var "f", Var "x")))
let church_2 = Lam ("f", Lam ("x", App (Var "f", App (Var "f", Var "x"))))
let church_3 = Lam ("f", Lam ("x", App (Var "f", App (Var "f", App (Var "f", Var "x")))))

(* Church arithmetic *)
let church_succ = Lam ("n", Lam ("f", Lam ("x", App (Var "f", App (App (Var "n", Var "f"), Var "x")))))
let church_add = Lam ("m", Lam ("n", Lam ("f", Lam ("x", App (App (Var "m", Var "f"), App (App (Var "n", Var "f"), Var "x"))))))
let church_mult = Lam ("m", Lam ("n", Lam ("f", App (Var "m", App (Var "n", Var "f")))))

(* Test examples *)
let test_krivine () =
  Printf.printf "Krivine Machine Tests:\n";
  
  (* Test identity function application with closed term *)
  let expr1 = App (id, church_1) in
  let (result1, _) = Krivine.eval_expr expr1 in
  Printf.printf "id church_1 = %s\n" (Krivine.string_of_expr (Krivine.unload result1));
  
  (* Test church numerals: succ 1 *)
  let expr2 = App (church_succ, church_1) in
  let (result2, _) = Krivine.eval_expr expr2 in
  Printf.printf "succ 1 = %s (should be equivalent to church_2)\n" 
    (Krivine.string_of_expr (Krivine.unload result2));
  
  (* Test application of true/false *)
  let expr3 = App (App (true_lam, Var "x"), Var "y") in
  let (result3, _) = Krivine.eval_expr expr3 in
  Printf.printf "true x y = %s\n" (Krivine.string_of_expr (Krivine.unload result3));
  
  let expr4 = App (App (false_lam, Var "x"), Var "y") in
  let (result4, _) = Krivine.eval_expr expr4 in
  Printf.printf "false x y = %s\n" (Krivine.string_of_expr (Krivine.unload result4));
  
  (* Test conditionals *)
  let expr5 = App (App (App (if_then_else, true_lam), church_1), church_0) in
  let (result5, _) = Krivine.eval_expr expr5 in
  Printf.printf "if true then church_1 else church_0 = %s\n" 
    (Krivine.string_of_expr (Krivine.unload result5))


    (* ---------- Additional Testing Code ---------- *)

let test_secd () =
  Printf.printf "\nSECD Machine Tests:\n";
  
  (* Test identity function application with closed term *)
  let expr1 = App (id, church_1) in
  let state1 = SECD.eval_expr expr1 in
  let result1 = SECD.get_result state1 in
  Printf.printf "id church_1 = %s\n" (SECD.string_of_value result1);
  
  (* Test church numeral: succ 1 *)
  let expr2 = App (church_succ, church_1) in
  let state2 = SECD.eval_expr expr2 in
  let result2 = SECD.get_result state2 in
  Printf.printf "succ 1 = %s (should be equivalent to church_2)\n" (SECD.string_of_value result2);
  
  (* Test application of true and false *)
  let expr3 = App (App (true_lam, Var "x"), Var "y") in
  let state3 = SECD.eval_expr expr3 in
  let result3 = SECD.get_result state3 in
  Printf.printf "true x y = %s\n" (SECD.string_of_value result3);
  
  let expr4 = App (App (false_lam, Var "x"), Var "y") in
  let state4 = SECD.eval_expr expr4 in
  let result4 = SECD.get_result state4 in
  Printf.printf "false x y = %s\n" (SECD.string_of_value result4);
  
  (* Test conditional: if true then church_1 else church_0 *)
  let expr5 = App (App (App (if_then_else, true_lam), church_1), church_0) in
  let state5 = SECD.eval_expr expr5 in
  let result5 = SECD.get_result state5 in
  Printf.printf "if true then church_1 else church_0 = %s\n" (SECD.string_of_value result5)

(* Main entry point to run tests *)
let () =
  test_krivine ();
  test_secd ()
