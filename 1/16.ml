let path (a,b) =
  let n = a + b in
  let rec comb1 (n,m) =
    if n = m || m = 0 then 1
    else comb1 (n,m-1)*(n-m+1)/m
    ;;
