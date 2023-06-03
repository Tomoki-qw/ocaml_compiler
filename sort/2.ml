(*目的：第一引数の組を (a,b) として，第二引数で示すリストの要素が a ならば b に置換する関数を定義する.*)
(*substitution : 'a * 'a -> 'a list -> 'a list*)
let rec substitution (a,b) lst = match lst with [] -> []
| x :: rest -> if a = x then b :: (substitution (a,b) rest) else x :: (substitution (a,b) rest);;
(*テスト*)
let test1 = substitution ("good","best") ["better";"good";"best"] =["better";"best";"best"]
let test2 = substitution (2,4) [1;2;3;4] =[1;4;3;4]
let test3 = substitution (1,2) [3;4;5;6;1;2] =[3;4;5;6;2;2]
let test4 = substitution (1,2) [] = []
