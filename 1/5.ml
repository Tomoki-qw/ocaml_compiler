let rec add2list list =
  match list with [] -> []
  | [b] -> []
  | a :: b :: rest1 -> (a + b) :: add2list (b :: rest1)
  ;;