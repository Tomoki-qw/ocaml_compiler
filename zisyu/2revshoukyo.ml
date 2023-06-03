(*いろんな場所で使う関数を先に定義する*)
(*リストの要素数を返す*)
let rec length lst =
  match lst with [] -> 0
  | _ :: rest -> 1 + (length rest) 
let kyori lst = length lst
(*2つのリストを結合する*)
let rec append l1 l2 =
  match l1 with [] -> l2
  | first :: rest -> first :: (append rest l2)
(*リストを反転させる*)
let rec reverse lst =
  match lst with [] -> []
  | first :: rest -> append (reverse rest) [first]
(*n個のリストを結合する*)
let rec concat lst =
  match lst with [] -> []
  | n :: rest -> append n (concat rest)
(*リストの全要素にそれぞれ関数fで定義された処理を行う*)
let rec map f lst =
  match lst with 
  [] -> []
  | v :: rest -> f v :: map f rest
(*リストyの先頭に要素xを追加する*)
let cons x y =(::) (x,y);;

(*以下、最長経路長とそのルートを出力するという目標に向かって関数を組んでいく。方針としては、
与えられた座標のリストをlistとすると、list内の座標のすべての経路を求める
->その中の経路長が同じ経路を削除する->残った経路のすべての経路長を計算する
->その中から最長の値と、その時の経路を出力する*)

(* lstの各要素xと，xより左にある要素のリスト，xより右にある要素のリストを列挙する.
例えばselect [1;2;3] ;; では [([],1,[2;3]) ; ([1],2,[3]) ; ([1;2],3,[])] が出力される.*)
let rec select ls_empty lst = match lst with
  | [] -> [] 
  | x :: rest -> (reverse ls_empty, x, rest) :: select (x :: ls_empty) rest
let select lst = select [] lst
let rec retsu lst =
  let n = length lst in
(* リストから要素n個を選えらんでその全順列を出力する *)
let rec permutation n lst =
  match n, lst with
  | 0, _ -> [[]]
  | _, [] -> []
  | n, lst ->
      concat (
      map 
        (fun (a, x, b) ->
          map 
            (cons x) 
            (permutation (n - 1) (concat [a;b]))) (
        select lst )) in
      permutation n lst
;;
(*順周りでルートが同じ順番を省く
例えば(0,0)->(2,2)->(3,3)->(0,0)と(2,2)->(3,3)->(0,0)->(2,2)は同じ経路長なので片方消す*)
let rec fact n =
  if n = 1 then 1 else fact (n-1) * n
let shoukyo lst =
  let lst2 = (retsu lst) in
  let k = 1 in
  let n = length lst in
    let rec in_shoukyo k lst2 =
    match lst2 with [] ->[]
    | x :: rest ->
    if k > (fact (n-1)) then []
    else x :: (in_shoukyo (k+1) rest) in
in_shoukyo k lst2
    ;;
(*リスト内の最初の要素を取り出し、それを最後に追加する*)
let first lst =
  match lst with [] -> failwith "error"
  |x :: rest -> x
;;

let tsuika lst =
  let lst_hanten = reverse lst in
  (first lst) :: lst_hanten;;

  let rec alltsuika lst =
    map tsuika (shoukyo lst)
    ;;
(*逆回りで同じルートを辿る経路を省く
例えば(0,0)->(2,2)->(3,3)->(0,0)と(0,0)->(3,3)->(2,2)->(0,0)は同じ経路長なので片方消す*)
    let rev_shoukyo lst =
      let lst = alltsuika lst in
      let rec in_rev_shoukyo lst =
        match lst with [] -> []
        |[x] -> [x]
        |x :: y :: [] -> if reverse x = y then [x] else [x;y]
        |x :: y :: rest -> if reverse x = y then x :: (in_rev_shoukyo rest) 
                           else (in_rev_shoukyo (x :: (append rest [y])))
        in in_rev_shoukyo lst
        ;;
(*一周の経路長を求める*)
let kyori ((a,b),(c,d)) =
  let zyou n = n *. n in
  sqrt(zyou(float_of_int(a - c)) +. zyou(float_of_int(b - d))) 
let rec keirocho lst =
  match lst with [] -> 0.
  | [x] -> 0.
  | x :: y :: rest -> kyori (x,y) +. (keirocho (y :: rest))
  ;; 
  (*各ルートの経路長をリストにする*)
  let all_keirocho lst =
    let lst = rev_shoukyo lst in
    let rec in_all_keirocho lst =
    match lst with [] -> []
    | x :: rest -> keirocho x :: (in_all_keirocho rest)
    in in_all_keirocho lst
  ;;
  (*経路長のリストから最長のものを取り出す*)
  let max_keirocho lst =
    let lst = all_keirocho lst in
  let rec max lst =
  match lst with [] -> failwith "error1"
  |[x] -> x
  |x :: y :: rest ->
  if x > y then max (x :: rest)
  else max (y :: rest) in
  max lst
  ;;
  (*最長経路長となるルートを出力*)
let max_route lst =
  let last = rev_shoukyo lst in
  let rec in_max_route last =
  match last with [] ->[]
  | x :: rest ->
  if (keirocho x) = (max_keirocho lst) then x
  else in_max_route rest in
  in_max_route last
  ;; 