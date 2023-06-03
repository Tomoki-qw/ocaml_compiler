let rec merge list1 list2 =
  match (list1,list2) with ([],[]) -> []
  | ([],list2) -> list2
  | (list1,[]) -> list1
  | (a :: rest1,b :: rest2) -> a :: b :: merge rest1 rest2
  ;;