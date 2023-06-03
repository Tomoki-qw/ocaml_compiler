let rec replicate a list =
  match (a,list) with (0,list) ->[]
  |(x,list) -> list :: replicate (x-1) list
  ;;