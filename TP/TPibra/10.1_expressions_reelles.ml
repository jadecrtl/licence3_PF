(* GRADE:  100% *)
let string_of_uop op =
  match op with 
  |Cos ->"cos"
  |Sin->"sin"
    
    
let string_of_bop op =
  match op with 
  |Plus->"+"
  |Moins->"-"
  |Fois->"*"

let rec string_of_expr e =
  match e with 
  |Const f-> string_of_float f
  |Var s-> s
  |Uop (uop,exp)-> string_of_uop uop ^ "(" ^ string_of_expr exp ^")"
  |Bop (bop,e1,e2)-> "("^ string_of_expr e1 ^" " ^ string_of_bop bop ^" "^  string_of_expr e2 ^")"

let eval_uop op =
  match op with 
  |Cos-> cos 
  |Sin->sin 
    
let eval_bop op =
  match op with 
  |Plus-> (fun x y-> x+.y)
  |Moins->(fun x y-> x-.y) 
  |Fois ->(fun x y-> x*.y)

let rec eval_expr env e =
  match e with 
  |Const f->f
  |Var s->List.assoc s env 
  |Uop (uop,exp)->eval_uop uop (eval_expr env exp)
  |Bop (bop,e1,e2)-> eval_bop bop (eval_expr env e1) (eval_expr env e2) 
    
    

