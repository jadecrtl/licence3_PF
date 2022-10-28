(* GRADE:  100% *)
let rec string_of_formula f =
  match f with 
  |Prop p-> p
  |Neg (phi)->"Neg "^ string_of_formula phi
  |And(phi1,phi2)->"("^ string_of_formula phi1 ^" And "^string_of_formula phi2^ ")"
  |Or(phi1,phi2)->"("^ string_of_formula phi1 ^" Or "^string_of_formula phi2^ ")";;
string_of_formula (And (Prop "p", Neg (Or (Prop "q", Prop "r"))));;
string_of_formula (Or (Prop "a", Prop "b"));;

let rec list_of_props f =
  
  
  match f with 
  |Prop p-> [p]
  |Neg(phi1)->list_of_props phi1
  |And(phi1,phi2)->union_sorted (list_of_props phi1)(list_of_props phi2)
  |Or(phi1,phi2)->union_sorted (list_of_props phi1)(list_of_props phi2);;


let rec eval_formula f l =
  match f with 
  |Prop p->(match List.assoc p l with 
      |false->false
      |true->  true
    )
  |Neg phi->not(eval_formula phi l)
  |And(phi1,phi2)->  (eval_formula phi1 l && eval_formula phi2 l)
  |Or(phi1,phi2)->  (eval_formula phi1 l || eval_formula phi2 l);;                    

