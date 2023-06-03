(*第二引数のリストを逆順にして第一引数のリストと結合する関数を定義する.ただし，両リストの長さが異なる
   場合は，例外処理を行なう.*)
let rec append l1 l2 =
  match l1 with [] -> l2
  | first :: rest -> first :: (append rest l2)
let rec reverse lst =
  match lst with [] -> []
  | first :: rest -> append (reverse rest) [first]
let rec length lst =
  match lst with [] -> 0
  | _ :: rest -> 1 + (length rest) 
let revm lst1 lst2 = if (length lst1) = (length lst2) then append lst1 (reverse lst2)
else failwith "error"
;;

