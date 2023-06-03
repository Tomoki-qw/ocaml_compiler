(*目的：引数で示すリストの要素で同じものは削除し，最後に出現した要素のみ残す関数を定義する.*)
(*unique : 'a list -> 'a list*)
let rec check a lst = match lst with [] -> false
|x :: rest -> if a = x then true else check a rest
let rec unique lst = match lst with [] -> []
|x :: rest -> if (check x rest) = true then unique rest else x :: unique rest;;
(*テスト*)
let test1 = unique [2;2;23;4;3;1;2] = [23;4;3;1;2]
let test2 = unique ["aa";"bb";"test";"aa";"dd";"cc"] = ["bb";"test";"aa";"dd";"cc"]