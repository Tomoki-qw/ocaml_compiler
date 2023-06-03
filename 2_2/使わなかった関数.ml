(*各座標の組の先頭に番号を振る*)
let bangou lst =
  let a = 1 in
  let rec in_bangou a lst =
    match lst with [] -> []
  | (c,b) :: rest -> (a,c,b) :: (in_bangou (a+1) rest) in
  in_bangou a lst
    ;;
  (*各組の先頭の番号のみ取り出してリストにする*)
  let toridashi lst =
    let lst = bangou lst in
    let rec in_toridashi lst =
    match lst with [] -> []
    | (a,b,c) :: rest -> a :: (in_toridashi rest) in
  in_toridashi lst 
    ;;