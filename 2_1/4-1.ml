let differ a b =
  let t1 = a (b +. 0.000000001) in
  let t2 = t1 -. a (b) in
  t2 /. 0.000000001
  ;;