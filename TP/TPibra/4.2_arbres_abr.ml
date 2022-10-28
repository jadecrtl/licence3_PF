(* GRADE:  100% *)
let rec contains_bst x a =
  match a with 
  |Nil->false
  |Node(e,a1,a2) when x= e-> true
  |Node(e,a1,a2) when x< e-> contains_bst x a1
  |Node (e,a1,a2)-> contains_bst x a2;;

let rec add_bst x a =
  match a with 
  |Nil-> Node(x,Nil,Nil)
  |Node(e,a1,a2) when x=e -> a
  |Node(e,a1,a2) when x<e -> Node (e,add_bst x a1,a2)
  |Node(e,a1,a2) -> Node(e,a1, add_bst x a2);;                             

let rec bst_of_list l =
  match l with 
  |[]->Nil
  |x::r->add_bst (x) (bst_of_list(r));;


let rec bst_of_list_opt l =
  let rec devise l d f=
    match l with 
    |[]->[]
    |x::r when d=0   && d<f ->x::devise r d (f-1)
    |x::r when d<f-> devise r (d-1) (f-1)
    |x::r ->[] in 
  let len= List.length l in 
  let mil = len/2 in 
  let l1= devise l 0 mil in 
  let l2 = devise l (mil+1) len in 
  if len=0 then Nil
  else Node(List.nth l mil, bst_of_list_opt l1, bst_of_list_opt l2);;
      



let rec is_bst a =
  let rec max_bst a=
    match a with 
    |Nil-> (min_int)
    |Node(e,fg,fd)-> max (max (e)(max_bst fg)) (max_bst fd) in 
  let rec min_bst a= 
    match a with 
    |Nil-> max_int
    |Node(e,fg,fd)-> min (min (e)(min_bst fg)) (min_bst fd) in 
  match a with 
  |Nil->true
  |Node(e,fg,fd) when (e<=max_bst fg)||(e>= min_bst fd)->false
  |Node(e,fg,fd)-> is_bst fg && is_bst fd;;
                       
