let differ f_1 a =
  (f_1 (a +. 0.000001) -. f_1 (a)) /. 0.000001
;;
let rec ext f_2 (b,c) =
  if b >= c then max_float
  else if differ f_2 b <= 0.000001 && differ f_2 b >= -0.000001 then f_2 b
  else let b_2 = b +. 0.0000001 in
  ext f_2 (b_2,c)
;;