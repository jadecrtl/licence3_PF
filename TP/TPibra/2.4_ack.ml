(* GRADE:  100% *)
let rec ack m n =
  match(m,n) with
  |(0,n)-> n+1
  |(m,0)-> ack(m-1)(1)
  |(m,n)-> ack(m-1)(ack (m)(n-1));;

ack 0 0;
ack 1 1;
ack 2 2;

