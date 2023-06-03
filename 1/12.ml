let rec append list1 list2 =
  match list1 with 
  [] -> list2
  |first :: rest -> first :: (append rest list2)
  let rec concat list =
    match list with [] -> []
    |n :: rest -> append n (concat rest)
    ;;