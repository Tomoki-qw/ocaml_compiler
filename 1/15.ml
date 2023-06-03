
let rec extract a list =
  match list with [] -> []
  |[x] -> 
  if (a) x then [x]
  else []
  | x :: rest -> if (a) x then x :: extract a rest
  else extract a rest
  ;;