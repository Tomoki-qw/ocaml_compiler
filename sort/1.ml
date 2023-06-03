(*目的：与えられた文字列のリスト全体の文字数をカウントする関数を定義する*)
(*countchar:string list -> int*)
let count lst = String.length lst
let rec countchar lst = match lst with [] -> 0
|x :: rest-> count x + countchar rest
;;
(*テスト*)
let test1 = countchar ["count";"char"] = 9
let test2 = countchar ["this";"is";"test2"] = 11
let test3 = countchar[] = 0
;;
  