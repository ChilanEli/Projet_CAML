(* TODO: Rewriting *)

open Lang;;
open Subst;;
open Unif;;
open Interf;;


(* simplify a term until no more rewrite step succeeds *)
let rec simp rls trspec t = t


(* Different rewrite strategies *)


let leftmost_outermost l r t (lrew, lch, ln) (rrew, rch, rn) (trew, tch, tn) =
  if tch
  then (trew, tch, tn)
  else if lch
  then (Appl(lrew, r), lch, ln)
  else (Appl(l, rrew), rch, rn)


let parse_and_simp infile = 
  match parse infile with
    Rewr (rls, trspec, sn, t) -> 
      print_term (simp rls trspec t)
;;

(* map strategy name to strategy *)
let map_strat_name = function
  | "left_outer" -> Eager leftmost_outermost
  | n -> failwith ("strategy " ^ n ^ " not implemented.")


(* TODO: the same function using strategies, see section 4.3.3
let parse_and_simp infile = 
  match parse infile with
    Rewr (rls, trspec, sn, t) -> 
      print_term (simp rls trspec (map_strat_name sn) t)
;;

*)

(* try_rewrite returns the rewritten term, and:
   - true if there is a change in the term
   - false if there is no change (even if rule was applicable without change)
*)

let try_rewrite l r t = 
  match unif_option t l with
  | None -> (t, false)
  | Some sb -> 
    let trew = apply_subst r sb in (trew, trew != t)

(* try to rewrite with rule list rls in term t,
   selecting the first applicable rule.
   Returns:
   the rewritten term, 
   true/false indicating change,
   the name of the applied rule (or empty string)
*)

let rec try_rewrite_rule_list rls t =
  match rls with
  | [] -> (t, false, "")
  | Rl(n, l, r) :: rls' -> 
    let (trew, tch) = try_rewrite l r t in
    if tch 
    then (trew, tch, n)
    else try_rewrite_rule_list rls' t

