(* GRADE:  100% *)
let rec size a =
  match a with
  |Nil -> 0
  |Node(_,a1,a2)->1+size a1+size a2;;

let rec depth a =
  match a with
  |Nil->0 
  |Node(_,a1,a2)->1+max(depth a1)(depth a2);;

let rec sum a =
  match a with 
  |Nil-> 0
  |Node(e,a1,a2)->e+sum a1 + sum a2;;

let rec contains x a =
  match a with 
  |Nil->false
  |Node(e,a1,a2) when e=x->true
  |Node(e,a1,a2)-> contains x a1 || contains x a2;;

let rec elements a =
  match a with
  |Nil-> []
  |Node(e,a1,a2)->elements a1 @ [e] @ elements a2;;

let rec elements a =
  let rec append l1 l2 =
    match l1 with 
    |[]->l2
    |x::r1-> x::append r1 l2 in 
  match a with 
  |Nil-> []
  |Node(e,a1,a2)->append (append (elements a1)( [e]))(elements a2);;

let rec perfect a =
  match a with 
  |Nil -> true 
  |Node (_,a1,a2) when depth a1=depth a2 -> perfect a1 && perfect a2 
  |Node(_,a1,a2)-> false;;

let perfect1 a =
  let rec perfect_depth a=
    match a with 
    |Nil->(true,0)
    |Node(_,a1,a2)-> 
        let pf1 = perfect_depth a1 in
        let pf2 = perfect_depth a2 in 
        (snd pf1 = snd pf2 && fst pf1 && fst pf2, 1+ max(snd pf1)(snd pf2))
  in fst(perfect_depth (a)) ;;
