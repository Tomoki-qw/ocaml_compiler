let rec kaidan n =
  if n = 1 then 1
  else if n = 2 then 2
  else kaidan (n-1) + kaidan (n-2)
  ;;