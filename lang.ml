(* -------------------- Data types ------------------------------ *)

type term = 
    Const of string
  | Var of string
  | Appl of term * term

type rule = Rl of string * term * term

type tracespec = 
    TraceAll 
  | TraceSome of string list
      
type rewr = Rewr of rule list * tracespec * string * term

type rewr_result = term * bool * string

type strategy = 
  Eager of 
      (term -> term -> term -> rewr_result -> rewr_result -> rewr_result -> rewr_result)
| Lazy of 
    (term -> term -> term ->
     (term -> rewr_result) -> 
     (term -> rewr_result) -> 
     (term -> rewr_result) -> rewr_result)


(* -------------------- Functions ------------------------------ *)

(* constructs term of reverse of list passed as argument *)
let rec term_of_term_list = function
  | [] -> failwith "empty term list"
  | [t] -> t
  | t::ts -> Appl (term_of_term_list ts, t)

let constr_pair t1 t2 = Appl (Appl (Const "pair", t1), t2)

let rec term_of_pair_term_list = function
  | [] -> failwith "empty pair term list"
  | [t] -> t
  | t::ts -> constr_pair t (term_of_pair_term_list ts)

let rec term_of_nat n = 
  if n = 0
  then Const "zero"
  else Appl (Const "succ", term_of_nat (n - 1))


let rec pair_list = function 
  | Appl (Appl (Const "pair", t1), t2) -> 
    t1 :: pair_list t2
  | t -> [t]

let in_paren t = "(" ^ t ^ ")"

let rec print_term_succ n = function 
  | Appl (Const "succ", t) -> print_term_succ (n + 1) t
  | Const "zero" -> string_of_int n
  | t -> 
    if n = 0 
    then print_term t
    else in_paren ("succ " ^ print_term_succ (n - 1) t)
and print_term = function 
  | Appl (Appl (Const "pair", t1), t2) ->             
    in_paren 
      (print_term t1 ^
         (List.fold_right (fun r t ->  ", " ^ r ^ t)
            (List.map print_term (pair_list t2)) ""))
  | Appl (Const "succ", t) -> print_term_succ 1 t
  | Appl (t1, t2) -> in_paren (print_term t1 ^ " " ^ print_term t2)
  | Const "zero" -> "0"
  | Const c -> c
  | Var x -> x


(* TODO *)
let print_trace_step trspec n trew = print_string "trace"
