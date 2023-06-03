let rec bubble list =
  match list with 
    [] -> []
    |x :: rest ->
      let rec bubble2 list =
        match list with [] -> []
          | [n] -> [n]
          | n :: rest1 -> 
          let m :: rest2 = (bubble2 rest1) in
          if n >= m then m :: n :: rest2
            else list
        in
  let min :: rest = (bubble2 list)
in min :: bubble rest
;;
  let test1 = bubble [1;2;3;4;4;3;2;1] = [1;1;2;2;3;3;4;4]
  let test2 = bubble [1;2;6;5;5;4;3] = [1;2;3;4;5;5;6]
  let test3 = bubble [] = []


