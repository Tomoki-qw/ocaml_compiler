module Vector = struct
exception Empty ;;
let vempty = [];;
let isvempty lst = 
  if lst = [] then true else false ;;  
let vector lst = lst;;
let rec vlength = function
[] -> 0
|x :: rest -> 1 + vlength rest
;;
let rec vshow = function
[] -> ()
| e :: l -> print_int e ; print_string "," ; vshow l ;;
let rec at a lst =
  match lst with [] -> raise Empty
  |x :: rest -> if a = 0 then x else at (a-1) rest
  ;;
end;;