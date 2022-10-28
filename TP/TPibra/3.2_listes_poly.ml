(* GRADE:  100% *)
(* Toutes les fonctions sont ici définies par des "let rec". Il est
   possible que dans vos solutions certaines fonctions ne soient pas
   récursives, car elles utilisent une fonction récursive auxiliaire
   définie localement. À vous d'enlever le mot clef "rec" dans ce
   cas. *)

let rec map f l =
  match l with 
  |[]->[] 
  |x::l-> f(x)::map f l;;
let succ x = x + 1 in map succ [1; 2; 3];;

let rec filter p l =
  match l with 
  |[]->[]
  |x::l when p(x)-> x:: filter p l
  |x::l -> filter p l;;
filter (function x -> x > 3) [4; 3; 10];;

let rec append l1 l2 =
  match l1 with 
  |[]->l2
  |x::r1-> x::append r1 l2 ;;

let rec rev l =
  match l with 
  |[]->[] 
  |x::l-> rev l @ [x];;

rev [1; 2; 3];;


let rec flatten l =
  match l with 
  |[]-> []
  |h::t -> append h (flatten t);;


let rec rotation_d l=
  let rec last l=
    match l with 
    |[]->[]
    |[x]->[x]
    |_::r-> last r in 
  let rec remove_last l=
    match l with 
    |[]->[]
    |[x]->[]
    |x::r->x::remove_last r in 
  let last= last l in 
  last@remove_last l;;
