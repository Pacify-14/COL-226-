(* Module: Terms, Signatures, Substitutions, and Unification in OCaml *)

module Term = struct
  (* Variable and Symbol types *)
  type variable = string
  type symbol   = string * int    (* name and arity stored, but signature defines true arities *)

  (* Pre-term and well-formed term representation using arrays *)
  type preterm =
    | V of variable
    | Node of symbol * preterm array

  (* A signature is a list of symbols with arities *)
  type signature = symbol list

  (* Check that a signature is valid: no duplicate names, non-negative arities *)
  let check_sig (sig_ : signature) : bool =
    let names = Hashtbl.create (List.length sig_) in
    List.for_all (fun (s, ar) ->
        ar >= 0 &&
        if Hashtbl.mem names s then false
        else ( Hashtbl.add names s (); true)
      ) sig_

  (* Lookup arity in signature *)
  let arity_of (sig_ : signature) (name : string) : int =
    try List.assoc name sig_ with Not_found -> -1

  (* Well-formedness: each Node's symbol occurs in signature and has correct number of children *)
  let rec wfterm (sig_ : signature) (t : preterm) : bool = match t with
    | V _ -> true
    | Node ((s, _), childs) ->
        let expected = arity_of sig_ s in
        expected >= 0 && Array.length childs = expected &&
        Array.for_all (wfterm sig_) childs

  (* Height of a term *)
  let rec ht t = match t with
    | V _ -> 0
    | Node (_, childs) ->
        if Array.length childs = 0 then 0
        else 1 + Array.fold_left (fun acc t' -> max acc (ht t')) 0 childs

  (* Size (# of nodes) of a term *)
  let rec size t = match t with
    | V _ -> 1
    | Node (_, childs) ->
        1 + Array.fold_left (fun acc t' -> acc + size t') 0 childs

  (* Variables appearing in a term: use String Set *)
  module VS = Set.Make(String)
  let rec vars t = match t with
    | V x -> VS.singleton x
    | Node (_, childs) ->
        Array.fold_left (fun acc t' -> VS.union acc (vars t')) VS.empty childs

end

module Subst = struct
  open Term
  (* A substitution is a mapping from variable to term, implemented as an assoc list for finite domain *)
  type t = (variable * preterm) list

  let empty : t = []
  let single x u : t = [(x, u)]

  (* Lookup or identity *)
  let rec apply_subst (s : t) (x : variable) : preterm = match s with
    | (y, u)::rest -> if x = y then u else apply_subst rest x
    | [] -> V x

  (* Homomorphic extension: substitute throughout the term *)
  let rec subst (s : t) (t0 : preterm) : preterm = match t0 with
    | V x -> apply_subst s x
    | Node (sym, childs) ->
        let childs' = Array.map (subst s) childs in
        Node (sym, childs')

  (* Compose substitutions: s2 after s1 *)
  let compose (s1 : t) (s2 : t) : t =
    (* apply s2 to images of s1, then include s2's own bindings not in s1 *)
    let mapped = List.map (fun (x, u) -> (x, subst s2 u)) s1 in
    mapped @ List.filter (fun (y, _) -> not (List.exists (fun (x, _) -> x = y) s1)) s2

end

module Unify = struct
  open Term
  open Subst
  exception NOT_UNIFIABLE

  (* Occurs-check: does variable x occur in term t? *)
  let rec occurs (x : variable) (t : preterm) : bool = match t with
    | V y -> x = y
    | Node (_, childs) -> Array.exists (occurs x) childs

  (* Unify two terms, returning an mgu as a substitution *)
  let mgu (t1 : preterm) (t2 : preterm) : t =
    let rec unify_pairs (subs : t) (pairs : (preterm * preterm) list) : t = match pairs with
      | [] -> subs
      | (u, v)::rest ->
          let u' = subst subs u in
          let v' = subst subs v in
          match (u', v') with
          | V x, V y when x = y -> unify_pairs subs rest
          | V x, _ -> if occurs x v' then raise NOT_UNIFIABLE else
                let s0 = single x v' in
                let subs' = compose subs s0 in
                unify_pairs subs' rest
          | _, V y -> unify_pairs subs ((V y, u')::rest)
          | Node ((f, ar1), c1), Node ((g, ar2), c2)
            when f = g && ar1 = ar2 ->
              let new_pairs = Array.to_list (Array.map2 (fun a b -> (a, b)) c1 c2) in
              unify_pairs subs (new_pairs @ rest)
          | _ -> raise NOT_UNIFIABLE
    in
    unify_pairs empty [(t1, t2)]

end

module Edit = struct
  open Term
  open Subst
  (* Positions as list of indices *)
  type pos = int list

  (* Functional replace: returns new term *)
  let rec edit (t : preterm) (pos : pos) (replacement : preterm) : preterm = match pos with
    | [] -> replacement
    | i::rest -> begin match t with
        | V _ -> failwith "Invalid position"
        | Node (sym, childs) ->
            if i < 0 || i >= Array.length childs then failwith "Index out of bounds"
            else
              let new_child = edit childs.(i) rest replacement in
              let new_arr = Array.copy childs in
              new_arr.(i) <- new_child;
              Node (sym, new_arr)
      end

  (* In-place substitution stub: needs mutable structure to work *)
  let rec subst_inplace (s : t) (t : preterm) : unit = match t with
    | V x -> (match apply_subst s x with
        | V y when y = x -> ()
        | _ -> failwith "In-place subst requires mutable nodes"
      )
    | Node (_, childs) -> Array.iter (subst_inplace s) childs

end

(* Test cases *)
let () =
  (* Signature checks *)
  assert (Term.check_sig [("a",1); ("b",0)]);
  assert (not (Term.check_sig [("a",1); ("a",2)]));

  (* Well-formedness *)
  let t = Term.Node (("f",2), [| Term.V "x"; Term.V "y" |]) in
  assert (Term.wfterm [("f",2)] t);
  assert (not (Term.wfterm [("f",1)] t));

  (* Height, size, vars *)
  assert (Term.ht t = 1);
  assert (Term.size t = 3);
  let vs = Term.vars t in
  assert (Term.VS.equal vs (Term.VS.of_list ["x";"y"]));

  (* Substitution and composition *)
  let s1 = Subst.single "x" (Term.Node (("c",0), [||])) in
  let t1 = Subst.subst s1 t in
  assert (match t1 with Node (("f",2), [| Term.Node (("c",0), _); _ |]) -> true | _ -> false);
  let s2 = Subst.single "y" (Term.V "z") in
  let s12 = Subst.compose s1 s2 in
  assert (List.length s12 = 2);

  (* Unification *)
  let u1 = Term.V "u" in
  let u2 = Term.Node (("g",1), [| Term.V "u" |]) in
  begin try ignore (Unify.mgu u1 u2); assert false with Unify.NOT_UNIFIABLE -> () end;
  let v1 = Term.V "x" and v2 = Term.V "y" in
  let su = Unify.mgu v1 v2 in
  assert (List.mem_assoc "x" su || List.mem_assoc "y" su);

  (* Edit *)
  let e = Term.Node (("h",2), [| Term.V "a"; Term.V "b" |]) in
  let e' = Edit.edit e [1] (Term.V "c") in
  assert (match e' with Node (_, [| _; Term.V "c" |]) -> true | _ -> false);

  print_endline "All tests passed!";
;;