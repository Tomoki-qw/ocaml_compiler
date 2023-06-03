let rec bubble list =
  match list with 
    [] -> []
    |x :: rest ->
      let rec bubble2 list =
        match list with 
          | [n] -> [n]
          | n :: rest1 -> 
          let m :: rest2 = (bubble2 rest1) in
          if n >= m then m :: n :: rest2
            else list
        in
  let min :: rest = (bubble2 list)
in min :: bubble rest
;;


