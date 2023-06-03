let rec chglist (a,b) list = 
  match list with [] -> []
  | x :: rest -> if a = x then b :: chglist (a,b) rest
  else x :: chglist (a,b) rest
  ;; 