(* GRADE:  100% *)
let insert x l =
  let rec insert_tail x l acc =
    match l with 
    |[]->(List.rev_append (acc) [x])
    |e::r when e=x -> List.rev_append (acc) l
    |e::r when x>e -> insert_tail x r (e::acc)
    |e::r-> List.rev_append (acc) (List.append [x;e] r) in insert_tail x l [];;

let sort l =
  let rec sort_tail l acc= 
    match l with 
    |[]->acc 
    |x::l'-> sort_tail l' (insert x acc) in sort_tail l [];;

let union_sorted l1 l2 =
  let rec union_sorted_tail l1 l2 acc= 
    match l1 with 
    |[]->List.rev_append acc l2
    |e1::r1-> match l2 with 
      |[]->List.rev_append acc l1
      |e2::r2 when e1=e2 -> union_sorted_tail r1 r2 (e1::acc)
      |e2::r2 when e1<e2 -> union_sorted_tail r1 l2 (e1::acc)
      |e2::r2 -> union_sorted_tail l1 r2 (e2::acc) in union_sorted_tail l1 l2 [];;
                   

let inter_sorted l1 l2 =
  let rec inter_sorted_tail l1 l2 acc= 
    match l1 with 
    |[]->List.rev acc
    |e1::r1-> match l2 with 
      |[]-> List.rev acc
      |e2::r2 when e1=e2-> inter_sorted_tail r1 r2 (e1::acc)
      |e2::r2 when e1<e2 -> inter_sorted_tail r1 l2 acc
      |e2::r2 -> inter_sorted_tail l1 r2 acc in inter_sorted_tail l1 l2 [];;

