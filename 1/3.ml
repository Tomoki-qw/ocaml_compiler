let rec dellt2 a list = 
  if a > 0 then 
  match (a,list) with (1,n :: rest) -> rest
  | (_,n :: rest) -> n :: dellt2 (a-1) rest
  | (_,[]) -> failwith "Error"
  else failwith "Error"
  ;;