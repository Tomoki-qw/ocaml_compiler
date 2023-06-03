let max_route lst =
  let last = alltsuika lst in
  let rec in_max_route last =
  match last with [] ->[]
  | x :: rest ->
  if (keirocho x) = (saicho_keirocho lst) then x
  else in_max_route rest in
  in_max_route last
  ;; 