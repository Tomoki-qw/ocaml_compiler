(*第 1 引数と第 2 引数にリストを渡し，要素が等しい時 true，それ以外の時 false を返す (順不同) 関数を定義
せよ.*)
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
let rec setequal lst1 lst2 = 
  let a = (merge_sort lst1) and b = (merge_sort lst2) in
  if a = b then true else false
  ;;
(*テスト*)
let test1 = setequal [1;2;3;4] [4;3;2;1] = true
let test2 = setequal ["aa";"bb";"cc"] ["aa";"bb"] =false;;