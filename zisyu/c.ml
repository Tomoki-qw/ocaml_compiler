let rec length lst =
  match lst with [] -> 0
  | _ :: rest -> 1 + (length rest) 
let kyori lst = length lst
let rec append l1 l2 =
  match l1 with [] -> l2
  | first :: rest -> first :: (append rest l2)
let rec reverse lst =
  match lst with [] -> []
  | first :: rest -> append (reverse rest) [first]
let rec concat lst =
  match lst with [] -> []
  | n :: rest -> append n (concat rest)
let rec map f lst =
  match lst with 
  [] -> []
  | v :: rest -> f v :: map f rest
let cons x y =(::) (x,y);;
let bangou lst =
let a = 1 in
let rec in_bangou a lst =
  match lst with [] -> []
| (c,b) :: rest -> (a,c,b) :: (in_bangou (a+1) rest) in
in_bangou a lst
  ;;
let toridashi lst =
  let lst = bangou lst in
  let rec in_toridashi lst =
  match lst with [] -> []
  | (a,b,c) :: rest -> a :: (in_toridashi rest) in
in_toridashi lst 
  ;;
let rec select ls_empty lst = match lst with
  | [] -> [] 
  | x :: rest -> (reverse ls_empty, x, rest) :: select (x :: ls_empty) rest
let select lst = select [] lst
let rec retsu lst =
  let n = length lst in
let rec permutation n lst =
  match n, lst with
  | 0, _ -> [[]]
  | _, [] -> []
  | n, lst ->
      concat (
      map (fun (a, x, b) ->
        map (cons x) (
        permutation (n - 1) (concat [a;b]))) (
      select lst )) in
      permutation n lst
;;
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
let kyori ((a,b),(c,d)) =
  let zyou n = n *. n in
  sqrt(zyou(float_of_int(a - c)) +. zyou(float_of_int(b - d))) 
let rec keirocho lst =
  match lst with [] -> 0.
  | [x] -> 0.
  | x :: y :: rest -> kyori (x,y) +. (keirocho (y :: rest))
  ;; 
  let all_keirocho lst =
    let lst = alltsuika lst in
    let rec in_all_keirocho lst =
    match lst with [] -> []
    | x :: rest -> keirocho x :: (in_all_keirocho rest)
    in in_all_keirocho lst
  ;;
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
let max_route lst =
  let last = alltsuika lst in
  let rec in_max_route last =
  match last with [] ->[]
  | x :: rest ->
  if (keirocho x) = (max_keirocho lst) then x
  else in_max_route rest in
  in_max_route last
  ;; 