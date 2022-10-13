let add_to_all x ll =
  List.map(fun ll -> x::ll) ll;;

let rec interpretations_props l =
  match l with
    [] -> [[]]
  | v::n ->
      let le = interpretations_props n in
      (add_to_all (v,false) le) @
      (add_to_all (v,true)  le);;

let interpretations f =
  interpretations_props (list_of_props f);;

let satisfiable f =
  let le = interpretations f in
  List.exists (eval_formula f) le;;

let tautology f =
  let le = interpretations f in 
  List.for_all (eval_formula f) le;;