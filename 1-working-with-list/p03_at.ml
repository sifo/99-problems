
let rec at n l =
  match l with
  | [] -> None
  | hd :: tail when n > 0 -> if n = 1 then Some hd else at (n-1) tail
  | _ -> None ;;

at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
at 3 [ "a" ; "b"; "c"];;
at 3 [ "a" ];;
at 1 [ "a" ];;
at 3 [ ];;
at (-3) [ "a" ; "b"; "c"];;

let rec at' n l =
  if n < 1
  then None
  else
    match l with
    | [] -> None
    | hd :: tl -> if n = 1 then Some hd else at (n-1) tl;;

at' 3 [ "a" ; "b"; "c"; "d"; "e" ];;
at' 3 [ "a" ; "b"; "c"];;
at' 3 [ "a" ];;
at' 1 [ "a" ];;
at' 3 [ ];;
at' (-3) [ "a" ; "b"; "c"];;
