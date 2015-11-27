(*
	Chilan-Eli YUKSEL
*)
(* Representation of substitutions as assoc lists *)


open Lang;;

let rec union = fun s1 -> fun s2 ->
  match s1 with
      [] -> s2
    | e::s1' -> 
      if List.mem e s2 
      then (union s1' s2)
      else e:: (union s1' s2)


let rec fv = function
  | Const c -> []
  | Var v -> [v]
  | Appl (t1, t2) -> union (fv t1) (fv t2)

let const_term t = (fv t = [])

let rec lookup_assoc = fun k -> fun kvs -> 
  match kvs with
      [] -> None
    | (k', v') :: kvs' -> 
	if k' = k then  Some v' else lookup_assoc k kvs'

let rec apply_subst = fun trm -> fun sbst ->
  match trm with
    | Const c -> Const c
    | Var v -> 
	(match lookup_assoc v sbst with
	     None -> Var v
	   | Some trm' -> trm')
    | Appl (t1, t2) -> Appl (apply_subst t1 sbst, apply_subst t2 sbst)


(* compose substitutions sbst' and sbst.
   The domains of sbst' and sbst must be disjoint,
   and the terms of sbst' must not contain vars of the dom of sbst
*)
let comp_subst sbst' sbst = 
  (List.map (fun (v, t) -> (v, apply_subst t sbst')) sbst) @ sbst'


