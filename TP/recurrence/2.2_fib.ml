let rec fibo n =
  if n = 0 then
    1
  else if n = 1 then 
    1 
  else 
    fibo (n - 1) + fibo (n -2)
;;

