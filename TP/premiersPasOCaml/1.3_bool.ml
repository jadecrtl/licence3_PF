let is_zero x =
  if x = 0 then true else false;; 

let msg_zero x =
  if x = 0 then "zero" else "not zero";;

let my_max a b =
  if a < b then b else a;;

let max_triple a b c =
  my_max (my_max a b) c;;

let max_quadruple a b c d =
  my_max (my_max (my_max a b) c) d;; 