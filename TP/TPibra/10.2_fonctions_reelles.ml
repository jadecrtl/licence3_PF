(* GRADE:  100% *)
let rec string_of_expr e =
  match e with 
  |Const f-> string_of_float f
  |Var s-> s
  |Uop (uop,exp)-> string_of_uop uop ^ "(" ^ string_of_expr exp ^")"
  |Bop (bop,e1,e2)-> "("^ string_of_expr e1 ^" " ^ string_of_bop bop ^" "^  string_of_expr e2 ^")"
  |App(f,e)->"("^ string_of_fonction f ^")"^ "("^ string_of_expr e ^ ")"

and string_of_fonction f =
  f.param ^ " |-> " ^ string_of_expr (f.corps);;

let rec eval_expr env e =
  match e with 
  |Const f->f
  |Var s->List.assoc s env 
  |Uop (uop,exp)->eval_uop uop (eval_expr env exp)
  |Bop (bop,e1,e2)-> eval_bop bop (eval_expr env e1) (eval_expr env e2) 
  |App(f,e)-> eval_fonction env f (eval_expr env e)

and eval_fonction env f arg =
  (*il faut juste supprimer f.param de env s'il existe, j'ai pas encore
  ajouté ça *)
  let env2 = (f.param, arg) :: env
  in
  eval_expr env2 f.corps 
      
let simpl_plus e1 e2 =
  match (e1,e2) with 
  |(Const c1,Const c2)-> Const (c1+.c2)
  |(Const 0., _)-> e2
  |(_,Const 0.)->e1 
  |(_,_)-> Bop (Plus,e1,e2);;

let simpl_moins e1 e2 =
  match (e1,e2) with 
  |(Const c1,Const c2)-> Const (c1-.c2)
  |(_, _) when e1=e2-> Const 0.
  |(_,Const 0.)->e1 
  |(_,_)-> Bop (Moins,e1,e2);;

let simpl_fois e1 e2 =
  match (e1,e2) with 
  |(Const c1,Const c2)-> Const (c1*.c2)
  |(Const 0., _)-> Const 0.
  |(_,Const 0.)->Const 0.
  |(Const 1., _)-> e2
  |(_,Const 1.)->e1       
  |(_,_)-> Bop (Fois,e1,e2);;

let simpl_bop op e1 e2 =
  match op with 
  |Plus-> simpl_plus e1 e2
  |Moins->simpl_moins e1 e2
  |Fois->simpl_fois e1 e2;;

let simpl_uop op e =
  match e with 
  |Const x-> Const (eval_uop op x)
  |_->Uop (op, e) ;;

let rec simpl_expr e =
  match e with 
  |Uop (op,exp)-> simpl_uop op (simpl_expr exp)
  |Bop (op,exp1,exp2)->simpl_bop op (simpl_expr exp1) (simpl_expr exp2) 
  |App (f,expr)->App((simpl_fonction f),(simpl_expr expr))  
  |_-> e

and simpl_fonction f =
  {param= f.param; corps= simpl_expr (f.corps)} 
