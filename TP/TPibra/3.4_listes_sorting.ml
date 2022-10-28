(* GRADE:  100% *)
let rec insert x l =
  match l with
  |[]->[x]
  |e::r when e=x->l
  |e::r when e<x->e:: insert x r
  |e::r -> x::[e]@r;;

let rec sort l =
  match l with 
  |[]->[]
  |x::r-> insert x (sort r);; 

let rec mem_sorted x l=
  match l with 
  |[]->false
  |[e]->if x=e then true else false
  |e::r->let rec devise l d f=
           match l with 
           |[]->[]
           |x::r when d=0   && d<f ->x::devise r d (f-1)
           |x::r when d<f-> devise r (d-1) (f-1)
           |x::r ->[] in 
      let len= List.length l in 
      let mil = len/2 in 
      if List.nth l mil= x then true 
      else 
      if List.nth l mil > x then mem_sorted x (devise l 0 (mil)) 
      else mem_sorted x (devise l (mil) (len+1));;

       
let rec union_sorted l1 l2 =
  match l1 with
  |[]->l2 
  |e1::r1-> match l2 with
    |[]->l1
    |e2::r2 when e1=e2 ->e1::union_sorted r1 r2
    |e2::r2 when e1<e2 ->e1::union_sorted r1 l2
    |e2::r2->e2:: union_sorted l1 r2;;

union_sorted [1; 3; 5] [2; 5; 8];;

let rec inter_sorted l1 l2 =
  match l1 with
  |[]->[]
  |e1::r1-> match l2 with 
    |[]->[]
    |e2::r2 when e1=e2 ->e1:: inter_sorted r1 r2
    |e2::r2 when e1<e2 ->inter_sorted r1 l2
    |e2::r2->inter_sorted l1 r2;;              



let rec quicksort l =
  match l with 
  |[]->[]
  |[x]->[x]
  |x::r->let rec partition l x=
           match l with 
           |[]->([],[])
           |e::l -> let a=partition l x in 
               if e<x then (e::fst a ,snd a)
               else (fst a,e::snd a) in  let a = partition r x in 
      quicksort (fst a)@ [x] @ quicksort (snd a);; 

