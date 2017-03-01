(* note: not tail-recursive *)
(* https://cseweb.ucsd.edu/classes/wi11/cse130/discussion/ocaml-tailrec.pdf *)
let rec compress = function
  | [] -> []
  | [x] -> [x]
  | x :: y :: tl ->
    if x = y
    then compress (y::tl)
    else x::compress(y::tl);;

(* note: the order of the match statements matter *)
(* note: not tail-recursive either *)
let rec compress' = function
  | x :: y :: tl ->
    if x = y
    then compress (y::tl)
    else x::compress(y::tl)
  | l -> l;;

let compress'' l =
  let rec aux acc = function
    | [] -> acc
    | [x] -> x :: acc
    | x :: y :: tl ->
      if x = y
      then aux acc (y::tl)
      else aux (x::acc) (y::tl)
  in List.rev (aux [] l);;

let compress''' l =
  let rec aux acc = function
    | x :: y :: tl ->
      if x = y
      then aux acc (y::tl)
      else aux (x::acc) (y::tl)
    | smaller -> smaller @ acc
  in List.rev (aux [] l);;

compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
compress' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
compress'' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
compress''' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
