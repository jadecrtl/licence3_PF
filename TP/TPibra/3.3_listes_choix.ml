(* GRADE:  100% *)

let choose l =
  List.nth l (Random.int (List.length(l)));;

choose [6; 5; 8; 7; 3];;
choose ['e';'c'; 'a'; 'd'; 'b'];;

let rec choose_elements l n = 
  match l with 
  |[]->[]
  |x::r ->match n with 
    |0->[]
    |_->
        x:: (choose_elements (r) (n-1));;   



let rec choose_sublist l n =
  match l with 
  |[]->[]
  |x::r ->match n with 
    |0->[]
    |_->x :: (choose_sublist (r) (n-1));; 
