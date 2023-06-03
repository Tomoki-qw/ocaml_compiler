(*目的：マージソートを行う関数を実装する*)
(*merge_sort : 'a list -> 'a list*)
let rec merge a b =
  match (a, b) with
    ([], _) -> b
  | (_, []) -> a
  | (x1:: a1, y1::b1) ->
    if x1 < y1 then x1::merge a1 b else y1::merge a b1
let rec length lst =
  match lst with [] -> 0
  | _ :: rest -> 1 + (length rest) 
  let rec sakuzyo a n =
    match (a, n) with
      ((_, 0) | ([], _)) -> a
    | (_ :: b, _) -> sakuzyo b (n - 1)
    let rec in_merge lst_length lst =
      match (lst_length,lst) with
      |(_,[]) -> []
      |(1,x::rest1) -> [x]
      |(2,x::rest1::rest2) -> if x<rest1 then [x;rest1] else [rest1;x]
      |(_,_) -> let relst_length = lst_length / 2 in
      merge (in_merge relst_length lst) (in_merge (lst_length - relst_length) (sakuzyo lst relst_length))
let rec merge_sort lst =
  let lst_length = length lst in
    in_merge lst_length lst
    ;;
(*テスト*)
let test1 = merge_sort [1;2;3;4;4;3;2;1] = [1;1;2;2;3;3;4;4]
let test2 = merge_sort [1;2;6;5;5;4;3] = [1;2;3;4;5;5;6]

