let daikei a b h =
  (a +. b) *. h /. 2.
;;
let rec integrate f a b =
  if a>= b then 0.
  else let inte1 = f(a) in
  let inte2 = f(a +. 0.0001) in
  (integrate f (a +. 0.0001) b ) +. (daikei inte1 inte2 0.0001)
  ;;