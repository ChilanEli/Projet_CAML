(*
	Chilan-Eli YUKSEL
*)
(* Unification *)

open Lang;;
open Subst;;

exception UnifError of string 

let rec unif = function
    ([], sbst) -> sbst
  | ((Const c1, Const c2) :: es, sbst) -> 
      if c1 = c2 then unif (es, sbst) else raise (UnifError("clash"))
  | ((Var x1, Var x2) :: es, sbst) -> 
      if x1 = x2 
      then unif (es, sbst) 
      else 
	let sbst' = [(x1, Var x2)] in 
	let es' = (List.map
                     (fun (s1,s2) -> (apply_subst s1 sbst', apply_subst s2 sbst'))
                     es) in
	unif (es', comp_subst sbst' sbst)
  | ((Appl (s1, s2), Appl (t1, t2)):: es, sbst) -> 
      unif ((s1, t1) :: (s2, t2) :: es, sbst)
  | ((Var x, t) :: es, sbst) ->
      if List.mem x (fv t) 
      then raise (UnifError("check"))
      else
	let sbst' = [(x, t)] in 
	let es' = (List.map 
                     (fun (s1,s2) -> (apply_subst s1 sbst', apply_subst s2 sbst'))
                     es) in
	unif (es', comp_subst sbst' sbst)
  | ((t, Var x) :: es, sbst) -> 
      unif ((Var x, t) :: es, sbst)
  | ((_, _) :: e, sbst) -> raise (UnifError("fail"))

let unif_option t1 t2 = 
  try Some (unif ([(t1, t2)], []))
  with UnifError(ue) -> None
