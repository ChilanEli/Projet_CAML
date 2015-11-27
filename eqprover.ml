(* Main function *)

open Lang;;
open Interf;;
open Rewriting;;

let main () = 
  let infile = Sys.argv.(1) in 
  match parse infile  with
    Rewr (rls, trspec, sn, t) -> 
      print_string ("Simplifying: " ^ (print_term t) ^ "\n");
      let s = (simp rls trspec t) in
      print_string ("Result: " ^ (print_term s) ^ "\n")
;;

main();;
