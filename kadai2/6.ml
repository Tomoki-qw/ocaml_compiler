let rec collatz n =
  if n = 1 then 1
  else if n mod 2 = 0 then collatz (n / 2)
  else collatz (n * 3 + 1)
  ;;