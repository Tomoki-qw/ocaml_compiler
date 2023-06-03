let author = ("Atsushi","Igarashi",174.0,61.0)
let big_tuple = ((3,'a'),(9.3,"Hello",false))
;;
let (firstname,lastname,height,weight) = author ;;
let (x,y) = big_tuple;;
let (f,s,b) = y;;

let (first,last,_,_) = author;; 
let (_,(_,s,_)) = big_tuple;;
let average p =
  let(x,y) = p in(x +. y) /. 2.0;;
let sum_and_diff (x,y) = (x +. y ,x -.y);;
