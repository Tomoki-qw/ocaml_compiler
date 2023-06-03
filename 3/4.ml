module Student:
sig
  type db
exception Not_found
exception Empty
val empty : db 
val add : int -> string -> db -> db 
val find : string -> db -> int
val min : db -> int
val find2 : int -> db -> string 
val delete : int -> db -> db 
val minsort : db -> db 
val show : db -> unit
val is_empty : db -> bool
end = struct
type db = Nil | Cell of int * string * db
exception Not_found ;;
exception Empty;;
let empty = Nil;;
let add a b c =
  Cell(a,b,c)
  ;;
let rec find s t =
  match t with 
  Nil -> raise Not_found
  |Cell(a,b,c) -> if b = s then a
  else (find s c)
  ;;
let rec min s =
  match s with Nil -> raise Empty
  |Cell(a,b,Nil) -> a
  |Cell(a,b,Cell(c,d,e)) -> if a < c then min (Cell(a,d,e)) else min (Cell(c,d,e))
  ;;
let rec find2 n t=
  match t with Nil -> raise Not_found
  |Cell(a,b,c) -> if a = n then b
  else (find2 n c)
  ;;
  
let rec delete n s =
  match s with Nil -> Nil
  |Cell (a,b,c) -> if a = n then delete n c
  else Cell(a,b,(delete n c))
  ;;

let rec minsort s =
  match s with Nil -> Nil
  |Cell(a,b,c) -> Cell((min s),(find2 (min s) s),(minsort (delete (min s) s)))
;;
let rec show a =
  let a = minsort a in
  match a with Nil -> print_newline ()
  |Cell(a,b,c) -> print_int a ; print_string ":" ; print_string b ; (if minsort c = Nil then print_string ":" else print_newline ()); show c
  ;;

let rec is_empty a =
  if a = Nil then true else false
  ;;
end;;