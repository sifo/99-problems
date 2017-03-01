
let rec last l =
  match l with
  | [] -> None
  | [e] -> Some e
  |  hd :: tl -> last tl;;

last [1; 2; 3; 4];;
last [];;

let rec last' = function
  | [] -> None
  | [e] -> Some e
  |  _ :: tl -> last tl;;

last' [1; 2; 3; 4];;
last' [];;
