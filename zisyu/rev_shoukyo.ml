let rec rev_shoukyo lst =
  match lst with [] -> []
  |x :: y :: [] -> if rev x = y then [x] else [x;y]
  |x :: y :: rest -> if rev x = y then x :: (rev_shoukyo rest) else y :: (rev_shoukyo (x :: rest))
  ;;