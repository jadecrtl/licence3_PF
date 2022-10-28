(* GRADE:  100% *)

let make_turtle x y = { x=x;y= y; angle=0.;path= Path.empty|>Path.moveto x y};;

let forward dist trace t = let pi= 4. *. atan 1. in 
  let conv a= (t.angle *. pi)/.180. in 
  
  let x=t.x+. (cos (conv t.angle) *. dist) in 
  
  let y=t.y+. (sin (conv t.angle) *. dist)in 
  
  if trace then 
    {x =x;
     y =y;
     angle=t.angle; path=t.path |>Path.lineto x y} 
  else 
    {x=x; y=y;
     angle=t.angle; path= t.path |>Path.moveto x y};;


let rec run cmds t = match cmds with 
  |[]-> t
  |c::cmd-> run cmd (command c t) and 
  
  command cmd t=match cmd with 
    |Line l-> forward l true t
    |Move m->forward m false t 
    |Turn turn->{x=t.x ;y= t.y; angle=t.angle+.turn;path=t.path} 
    |Repeat (0,cm)->      t     
    |Repeat (n,cm)->  
        command (Repeat(n-1,cm)) (run cm t) 
  
  
        


  
let triangle size = [Repeat (3,[Line size;Turn 120.] ) ];;

let square size = [Repeat (4,[Line size;Turn 90.] ) ];;

let polygon n size = [Repeat (n,[Line size;Turn (360./. (float_of_int n))] ) ];;

let spiral size factor angle n = let l = List.init (2*n) (fun i-> Line size) in
  List.mapi (fun i -> fun x -> if i mod 2 = 0 then
                (Line (size *. (Float.pow factor (float_of_int (i/2)))))
              else (Turn angle)
            ) l 

