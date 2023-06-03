
module BTree:
sig
  type 'a tree
  exception Empty;;
  val create : 'a tree
  val insert : 'a -> 'a tree -> 'a tree
  val search : 'a -> 'a tree -> bool
  val search_min : 'a tree -> 'a
  val search_max : 'a tree -> 'a
  val delete2 : 'a -> 'a tree -> 'b tree
  val delete : 'a -> 'a tree -> 'a tree
  val delete_min : 'a tree -> 'a tree
end 
= struct
type 'a tree = Nil | Node of 'a * 'a tree * 'a tree;;
exception Empty;;

let create = Nil;;

let rec insert a b = 
  match b with Nil -> Node(a,Nil,Nil)
  |Node (b1,b2,b3)-> if a < b1 then Node(b1,(insert a b2),b3) else Node(b1,b2,(insert a b3))
;;

let rec search a b =
  match b with Nil -> false
  |Node (b1,b2,b3) -> if a = b1 then true
  else if a < b1 then search a b2
  else search a b3
;; 

let rec search_min b =
  match b with Nil -> raise Empty
  |Node(b1,Nil,b3) -> b1
  |Node (b1,b2,b3) -> search_min b2
;;

let rec search_max b =
  match b with Nil -> raise Empty
  |Node(b1,b2,Nil) -> b1
  |Node (b1,b2,b3) -> search_max b3
;;

let rec delete2 a b =
  match b with Nil -> Nil
  |Node (b1,b2,b3) -> if a = b1 then Nil
  else if a < b1 then delete2 a b2
  else delete2 a b3
;; 

let rec delete a b =
  match b with Nil -> Nil
  | Node (b1,Nil,Nil) -> if a = b1 then Nil else raise Empty
  | Node (b1,b2,Nil) -> if a = b1 then b2
  else if a < b1 then Node(b1 ,(delete a b2) ,Nil) else Node(b1 ,b2 ,(delete a Nil)) 
  | Node (b1,Nil,b3) -> if a = b1 then b3
  else if a < b1 then Node(b1,(delete a Nil) ,Nil) else Node(b1,Nil,(delete a b3))
  | Node (b1,b2,b3) -> if a = b1 then Node ((search_min b3) ,b2 ,(delete2 (search_min b3) b3))
  else if a< b1 then Node(b1 ,(delete a b2) ,b3) else Node(b1 ,b2 ,(delete a b3))
;;

let delete_min b =
  let a = search_min b in
  delete a b
  ;;
end;;

