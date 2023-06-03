(*nの階乗*)
let rec fact n =
  if n = 1 then 1 else fact (n-1) * n;;
(*x+yの二重シグマの計算（例えばsi 3 3 ->(1+1)+(1+2)+(1+3)+(2+1)+...+(3+3)）*)
let rec ins a b =
  if b = 1 then a + 1
  else a + b + ins a (b-1)
;;
let rec si x y =
  if x = 0 then 0
  else ins x y + si (x-1) y
  ;;