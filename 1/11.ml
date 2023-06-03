let rec length list =
  match list with [] -> 0
  | _ :: rest -> 1 + (length rest)
  let rec inside_length list1 =
    match list1 with [] -> 0
    |n :: rest1 -> (length n ) + (inside_length rest1)
    ;;