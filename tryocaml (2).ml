(* Module: Terms, Signatures, Substitutions, and Unification in OCaml *)

module Term = struct
  (* Variable and Symbol types *)
  type variable = string
  type symbol   = string * int    (* name and arity *)

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
        else (Hashtbl.add names s (); true)
      ) sig_

  (* Lookup arity in signature *)
  let arity_of sig_ name =
    try List.assoc name sig_ with Not_found -> -1

  (* Well-formedness: each Node has correct number of children *)
  let rec wfterm sig_ = function
    | V _ -> true
    | Node ((s, ar), childs) ->
        Array.length childs = ar &&
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
  let rec apply_subst (s : t) x =
    match s with
    | (y, u)::rest -> if x = y then u else apply_subst rest x
    | [] -> V x

  (* Homomorphic extension: substitute throughout the term *)
  let rec subst (s : t) (t0 : preterm) : preterm =
    match t0 with
    | V x -> apply_subst s x
    | Node (sym, childs) ->
        let childs' = Array.map (subst s) childs in
        Node (sym, childs')

  (* Compose substitutions: s2 after s1 *)
  let compose (s1 : t) (s2 : t) : t =
    (* apply s2 to the images of s1, then append s2's others *)
    let mapped = List.map (fun (x, u) -> (x, subst s2 u)) s1 in
    mapped @ List.filter (fun (y, _) -> not (List.exists (fun (x, _) -> x = y) s1)) s2

end

module Unify = struct
  open Term
  open Subst
  exception NOT_UNIFIABLE

  (* Occurs-check: does variable x occur in term t? *)
  let rec occurs x = function
    | V y -> x = y
    | Node (_, childs) -> Array.exists (occurs x) childs

  (* Unify two terms, returning an mgu as a substitution *)
  let mgu t1 t2 =
    let rec unify_pairs (subs : t) = function
      | [] -> subs
      | (u, v)::rest ->
          let u' = subst subs u in
          let v' = subst subs v in
          match (u', v') with
          | V x, V y when x = y -> unify_pairs subs rest
          | V x, _            ->
              if occurs x v' then raise NOT_UNIFIABLE else
                let s0 = single x v' in
                let subs' = compose subs s0 in
                unify_pairs subs' rest
          | _, V y            -> unify_pairs subs ((V y, u')::rest)
          | Node ((f, ar1), c1), Node ((g, ar2), c2)
            when f = g && ar1 = ar2 ->
              let pairs = Array.to_list (Array.map2 (fun a b -> (a,b)) c1 c2) in
              unify_pairs subs (pairs @ rest)
          | _ -> raise NOT_UNIFIABLE
    in
    unify_pairs empty [(t1, t2)]

end

module Edit = struct
  open Term
  open Subst  (* for apply_subst, subst on variables *)
  (* Positions as list of indices *)
  type pos = int list

  (* Functional replace: returns new term *)
  let rec edit t pos replacement =
    match pos with
    | [] -> replacement
    | i::rest -> begin match t with
        | V _ -> failwith "Invalid position"
        | Node (sym, childs) ->
            if i < 0 || i >= Array.length childs then failwith "Index out of bounds" else
              let new_child = edit childs.(i) rest replacement in
              let new_arr = Array.copy childs in
              new_arr.(i) <- new_child;
              Node (sym, new_arr)
      end

  (* In-place substitution: mutate term array in situ *)
  let rec subst_inplace s t = match t with
    | V x -> (match apply_subst s x with
        | V y when y = x -> ()   (* identity: nothing to do *)
        | u ->
            (* Would require refs or mutable nodes; placeholder failure *)
            failwith "Cannot mutate V to Node in-place without mutable structure"
      )
    | Node (_, childs) -> Array.iter (subst_inplace s) childs

end

(* Example usage: *)
(*
open Term
let sig1 = [("0",0); ("1",0); ("+",2); ("*",2)] in
let t1 = Node (("+",2), [| Node (("0",0), [||]); V "x" |]) in
let t2 = Node (("+",2), [| V "y"; V "z" |]) in
let s = Unify.mgu t1 t2 in
Printf.printf "Unifier: %s\n" (String.concat ", " (List.map fst s));
*)
