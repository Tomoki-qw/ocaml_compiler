let rec check a list =
  match list with [] -> []
  | [h] -> 
  if a = h then []
  else [h]
  | h :: rest ->
  if a = h then rest
  else h :: check a rest
  ;;
let rec min list =
  match list with [] -> failwith "error"
  |[x] -> x
  |x :: y :: rest ->
  if x < y then min (x :: rest)
  else min (y :: rest)
  ;;
  let rec sentaku list =
    match list with [] -> []
    |x :: rest ->
    min list :: sentaku (check (min list) list)
    ;;
let test1 = sentaku [1;2;3;4;4;3;2;1] = [1;1;2;2;3;3;4;4]
let test2 = sentaku [1;2;6;5;5;4;3] = [1;2;3;4;5;5;6]
let test3 = sentaku [] = []