let rec bubble list =
  match list with [] -> []
  | [x] -> x :: bubble []
  | x :: y :: rest ->
  if x >= y then 
  y :: bubble (x :: rest)
  else x :: bubble (y :: rest)
;; 
(*let rec bubble list =
  let min :: rest = (sort1 list)
  in min :: bubble rest*)



