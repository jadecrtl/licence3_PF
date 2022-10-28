(* GRADE:  100% *)
(* Toutes les fonction sont ici définies par des "let rec". Il est
   possible que dans vos solutions certaines fonctions ne soient pas
   récursives, car elles utilisent une fonction recursive auxiliaire
   définie localement. À vous d'enlever le mot clef "rec" dans ce
   cas. *)

let rec list_length l =
  match l with
  |[]->0
  |_::l->1+list_length l;;

list_length[2;2;3];;

let rec list_product l =
  match l with 
  |[]->1
  |x::l->x*list_product l;;

list_product [2; 2; 3];;

let rec mem x l =
  match l with 
  |[]-> false
  |e::l-> 
      if x=e then true 
      else mem x l;;


let rec list_min l =
  match l with 
  |[]-> failwith "erreur, liste vide"
  |[x]->x
  |x::l-> min(x)(list_min l);;

list_min [-30; 2; 549];;

let rec last l =
  match l with 
  |[]-> failwith"liste vide"
  |[x]-> x
  |_::l->last l;;
last [1; 2; 3];;

let rec is_sorted l =
  match l with 
  |[]->true
  |[e1]->true
  |e1::e2::l-> if e1>e2 then false 
      else is_sorted (e2::l);;

is_sorted [1; 3; 5; 6; 4];;
is_sorted[1;2;3;4];;

  


let average l =
  let rec somme l=
    match l with
    |[]-> failwith"erreur"
    |[x]->x
    |x::l->x+somme l 
  in 
  somme l / list_length l;;
  

  

let rec nth l k =
  match l with 
  |[]-> failwith "vide"
  |x::l->if k=0 then x
      else nth (l)(k-1);;


let rec range n m =
  if n=m then n::[]
  else 
  if n<m then 
    n::range(n+1) (m)
  else  
    n::range(n-1)(m);;

