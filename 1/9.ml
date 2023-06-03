let rec inslist a b list =
  if a > 0 then 
  match (a,list) with (a,[]) -> b :: []
  |(1,list) -> b :: list
  |(a,x :: rest) -> x :: (inslist (a-1) b rest)
  else failwith "Error"
  ;;