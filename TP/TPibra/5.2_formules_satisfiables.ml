(* GRADE:  100% *)
let add_to_all x ll =
  List.map (fun l -> x::l) ll;;

let rec interpretations_props l =
  match l with 
  |[]->[[]] 
  |p::l-> let a= interpretations_props l in (add_to_all (p,false) a) @ (add_to_all (p,true) a) ;;


let interpretations f = 
  interpretations_props (list_of_props f) ;;
  

let satisfiable f =
  let inter= interpretations f in
  List.exists(eval_formula f) (inter);;

let tautology f =
  let inter= interpretations f in
  List.for_all(eval_formula f) (inter);;

