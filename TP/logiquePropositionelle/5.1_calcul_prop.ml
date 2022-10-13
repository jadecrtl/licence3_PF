let rec string_of_formula f =
  match f with
    Prop s -> s
  | Neg f -> "Neg" ^ (string_of_formula f)
  | And(f1,f2) -> "("^(string_of_formula f1)^" And "^(string_of_formula f2)^")"
  | Or(f1,f2) -> "("^(string_of_formula f1)^" Or "^(string_of_formula f2)^")";; 

let rec list_of_props f =
  match f with 
    Prop s -> [s]
  | Neg f -> list_of_props f 
  | And(f1,f2)
  | Or(f1,f2) -> union_sorted (list_of_props f1) (list_of_props f2);;  

let rec eval_formula f l =
  match f with
    Prop s -> (List.assoc s l)
  | Neg f1 ->  not (eval_formula f1 l)
  | And(f1,f2) -> (eval_formula f1 l) && (eval_formula f2 l)
  | Or(f1,f2) -> (eval_formula f1 l) || (eval_formula f2 l)
