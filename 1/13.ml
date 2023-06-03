let rec assoc aaa list =
  match list with [] -> failwith "Not found..."
  | (x,y) :: rest ->
  if x = aaa then y
  else if y = aaa then x
  else assoc aaa rest
  ;; 
