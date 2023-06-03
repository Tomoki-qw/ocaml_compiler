let rec minimum list =
  match list with [] -> failwith "Error"
  | [x] -> x
  | x :: y :: rest -> 
  if x > y then minimum (y :: rest)
  else minimum (x :: rest)
  ;;
