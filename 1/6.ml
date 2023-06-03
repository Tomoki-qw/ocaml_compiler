let rec mullist list1 list2 =
  match (list1,list2) with ([],[]) -> []
  |(a :: rest1, b :: rest2) -> (a * b) ::mullist rest1 rest2
  |([],rest2) -> failwith "Error"
  |(rest1,[]) -> failwith "Error"
  ;;