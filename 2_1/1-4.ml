(*マージソート*)

let rec in_bunkatsu_zenhan n lst =
  if n = 0 then []
  else match lst with [] -> []
  | x :: rest -> x :: (in_bunkatsu_zenhan (n-1) (rest))
  ;;
  let rec in_bunkatsu_kouhan n lst =
    if n = 0 then lst
  else match lst with [] -> []
  | x :: rest -> (in_bunkatsu_kouhan (n-1) (rest))
  ;;
  let rec in_bunkatsu n lst =
    [(in_bunkatsu_zenhan n lst);(in_bunkatsu_kouhan n lst)]
    ;;
  let rec bunkatsu lst =
    let n = (length lst)/2 in
    let rec in_loop lst =
      match lst with [] -> []
      |
(*bunkatsu...リストを分割　bunkatsu [4;3;2;1] -> [[4];[3];[2];[1]]*)
let rec bunkatsu lst =
  match lst with [] -> [[]]
  | [x] -> [[x]]
  | x :: rest -> [x] :: bunkatsu rest
;;
  (*hikaku...リスト内のリスト2つの最初ずつの大小を比較していく hikaku [4] [3] -> [3;4], hikaku [4;6] [5;7] -> [4;5;6;7]*)
let rec hikaku lst1 lst2 =
  match lst1 with [] -> lst2
  | x1 :: rest1 ->
    match lst2 with [] -> lst1
    | x2 :: rest2 -> 
      if x1 > x2 then x2 :: (hikaku (x1 :: rest1) rest2)
      else x1 :: (hikaku rest1 (x2 :: rest2))
;; 
  (*in_merge...hikakuを2つずつ全てに実行する in_merge [[1;3];[2;4];[5;7];[6;8]] -> [[1;2;3;4];[5;6;7;8]]*)
let rec in_merge lst = 
  match lst with [] -> []
  | x :: [] -> [x]
  | x :: y :: rest -> (hikaku x y) :: in_merge rest
;;
  (*merge...リストが一つになるまでin_mergeを繰り返す。*)
let rec merge lst =
  let lst = bunkatsu lst in
  let rec in_loop lst =
    match lst with [] -> []
    | [x] -> x
    | x :: y :: rest -> 
      let lst2 = in_merge lst 
      in in_loop lst2
  in in_loop lst
;;


(*fun1...リストを分割　fun1 [4;3;2;1] -> [[4];[3];[2];[1]]*)
(*fun2...リスト内のリスト2つの最初ずつの大小を比較していく fun2 [4] [3] -> [3;4], fun2 [4;6] [5;7] -> [4;5;6;7]*)
(*fun3...fun2を2つずつ全てに実行する fun3 [[1;3];[2;4];[5;7];[6;8]] -> [[1;2;3;4];[5;6;7;8]]*)
(*fun4...リストが一つになるまでfun3を繰り返す。*)