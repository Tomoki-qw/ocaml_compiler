(*"intersect l1 l2 = l1 かつ　l2"を満たす関数を定義する。*)
let rec append l1 l2 =
  match l1 with [] -> l2
  | first :: rest -> first :: (append rest l2)
let rec check a lst = match lst with [] -> false
|x :: rest -> if a = x then true else check a rest
let rec unique2 lst = match lst with [] -> []
|x :: rest -> if (check x rest) = true then x :: unique2 rest else unique2 rest;;
let intersect lst1 lst2 = 
  let mergelst = append lst1 lst2 in
  unique2 mergelst
  ;;
  (*テスト*)
  let test = intersect [23;34;45;56;67] [34;45;999] = [34;45]
  ;;