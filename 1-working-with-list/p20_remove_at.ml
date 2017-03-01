
let remove_at n l =
  let rec aux acc curr = function
  | [] -> List.rev acc
  | h :: t ->
    if curr = 0 then
      (List.rev acc) @ t
    else
      aux (h::acc) (curr-1) t
  in aux [] n l;;

remove_at 1 ["a";"b";"c";"d"];;
remove_at 2 ["a";"b";"c";"d"];;
remove_at 0 ["a";"b";"c";"d"];;
remove_at 10 ["a";"b";"c";"d"];;
remove_at (-2) ["a";"b";"c";"d"];;

(* not tail recursive *)
let rec remove_at n = function
  | [] -> []
  | h :: t ->
    if n = 0 then
      t
    else
      h :: remove_at (n-1) t;;

remove_at 1 ["a";"b";"c";"d"];;
remove_at 2 ["a";"b";"c";"d"];;
remove_at 0 ["a";"b";"c";"d"];;
remove_at 10 ["a";"b";"c";"d"];;
remove_at (-2) ["a";"b";"c";"d"];;
