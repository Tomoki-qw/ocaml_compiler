let rec dellt a list =
  if a >= 0 then 
  match (a,list) with (0,list) -> list
  | (_,n :: rest) -> dellt (a-1) rest
  | (_,[]) -> []
  else failwith "Error"
  ;;
