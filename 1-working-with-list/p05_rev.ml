
let rec rev = function
    [] -> []
  | hd :: tl -> (rev tl) @ [hd];;

rev ["a" ; "b" ; "c"];;
rev ["a"];;
rev [];;

let rev l =
  let rec call acc = function
    | [] -> acc
    | hd :: tl -> call (hd::acc) tl
  in
  call [] l;;

rev ["a" ; "b" ; "c"];;
rev ["a"];;
rev [];;
