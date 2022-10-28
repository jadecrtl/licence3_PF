(* GRADE:  100% *)

let matrix_map (f: 'a -> 'b) (m: 'a Matrix.t): 'b Matrix.t =
  Matrix.init (Matrix.size m) (fun (i,j)->f(Matrix.get m (i,j)))
;;


let gagnant_dim_2 (plateau : plateau): int option =
  gagnant_dim_1 (matrix_map (gagnant_dim_1) plateau)
;;
    

let termine_dim_2 (plateau : plateau) : bool =
  if gagnant_dim_2 plateau = None then 
    termine_dim_1  (matrix_map (fun x->
        if(termine_dim_1 x = true )then Some x
        else None) 
        plateau )
  else true
;;

