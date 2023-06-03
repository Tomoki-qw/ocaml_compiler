let rec check a list =
  match list with [] -> failwith "error3"
  | [h] -> 
  if a = h then []
  else [h]
  | h :: rest ->
  if a = h then rest
  else h :: check a rest
  ;;
let rec min list = (*minimum関数に書き換えれば昇順になる*)
  match list with [] -> failwith "error1"
  |[x] -> x
  |x :: y :: rest ->
  if x < y then min (x :: rest)
  else min (y :: rest)
  ;;
  let rec sentaku list =
    match list with [] -> failwith "error2"
    |[x] -> [x]
    |x :: rest ->
    min list :: sentaku (check (min list) list)
    ;;
(*選択ソート
minでリストから最小のものを見つける->それをリストの最初に持っていき
リストの残りでまた最小を見つけて…の繰り返し。
checkでリストから最小値を消してmin list ::で最初に持ってきている。*)