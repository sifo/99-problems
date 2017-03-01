
let rec rev l =
  match l with
  | [] -> []
  | hd :: tl -> (rev tl) @ [hd];;

let is_palindrome lst =
  (rev lst) = lst;;

let is_palindrome' lst =
  let rev l =
    let rec aux acc = function
      | [] -> acc
      | hd::tl -> aux (hd::acc) tl
    in
    aux [] l
  in
  rev lst = lst;;

is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
not (is_palindrome [ "a" ; "b" ]);;

is_palindrome' [ "x" ; "a" ; "m" ; "a" ; "x" ];;
not (is_palindrome' [ "a" ; "b" ]);;
