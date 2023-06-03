module List = struct
exception Empty;;
type list =
  Nil
  |Cell of int * list
  ;;
  let create = Nil;;
  let unshift x lst = Cell(x,lst);;
  let shift = function
  Nil -> raise Empty
  |Cell(x,lst) -> lst
  ;;

  let rec push x lst =
    match lst with Nil -> Cell(x,Nil)
    |Cell(a,b) ->Cell(a,(push x b));;

  let rec pop = function
  Nil -> raise Empty
  |Cell(x,Nil) -> Nil
  |Cell(x,y) ->Cell(x, (pop y))
  ;;

  let rec size = function
  Nil -> 0
  |Cell(x,y) -> 1 + size y
  ;;

  let rec max = function
  Nil -> raise Empty
  |Cell(x,Nil) -> x
  |Cell(x,(Cell(y,z))) ->if x > y then (max (Cell(x,z))) else (max (Cell(y,z)))
  ;;
  let rec min = function
  Nil -> raise Empty
  |Cell(x,Nil) -> x
  |Cell(x,(Cell(y,z))) -> if x < y then (min (Cell(x,z))) else (min (Cell(y,z)))
  ;;

  let rec get x lst = 
    match lst with Nil -> raise Empty
    |Cell(a,b) -> if x > 0 then get (x-1) b 
    else a ;;

  let indexOf x lst =
    let ind = 0 in
    let rec in_indexOf ind x lst =
      match lst with Nil -> -1
      |Cell(a,b) -> if a = x then ind else in_indexOf (ind+1) x b
    in in_indexOf ind x lst
    ;;

  let rec set x y lst=
    match lst with Nil -> Nil
    |Cell(a,b) -> if a = x then Cell(y,(set x y b)) else Cell(a,(set x y b))
    ;;

  let rec remove x lst =
    match lst with Nil -> Nil
    |Cell(a,b) -> if a = x then (remove x b) else Cell(a,(remove x b))
    ;;

    let rec concat lst1 lst2 =
      match lst1 with Nil -> lst2
      |Cell(x,y) -> Cell(x,(concat y lst2))
      ;;
    end;;
    