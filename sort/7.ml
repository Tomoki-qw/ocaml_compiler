(*"union l1 l2 = l1 または l2"を満たす関数を定義する*)
let rec append l1 l2 =
  match l1 with [] -> l2
  | first :: rest -> first :: (append rest l2)
let rec check a lst = match lst with [] -> false
|x :: rest -> if a = x then true else check a rest
let rec unique lst = match lst with [] -> []
|x :: rest -> if (check x rest) = true then unique rest else x :: unique rest;;
let union lst1 lst2 = 
  let mergelst = append lst1 lst2 in
  unique mergelst
  ;;
  (*テスト*)
  let test = union ["at";"then";"where"] ["then";"so";"what";"where"] = ["at";"then";"so";"what";"where"]
  ;;