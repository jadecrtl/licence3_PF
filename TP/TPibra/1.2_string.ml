(* GRADE:  100% *)
let bis s =
  s^s;;

let times8 s = 
  let s1=s^s in 
  let s2=s1^s1 in 
  let s3=s2^s2 in s3;;
  
let times8_bis s =
  bis(bis(bis s));;

