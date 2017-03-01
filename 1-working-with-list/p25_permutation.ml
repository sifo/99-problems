let rec at n l =
  match l with
  | [] -> None
  | hd :: tail when n > 0 -> if n = 1 then Some hd else at (n-1) tail
  | _ -> None ;;

let remove_at n l =
  let rec aux acc curr = function
  | [] -> List.rev acc
  | h :: t ->
    if curr = 0 then
      (List.rev acc) @ t
    else
      aux (h::acc) (curr-1) t
  in aux [] n l;;

let permutation lst =
  let rec aux acc = function
    | [] -> acc
    | l ->
      let r = Random.int (List.length l) in
      let e = match (at (r+1) l) with Some(x) -> x | _ -> assert false in
      let sl = remove_at r l in
      aux (e::acc) sl
  in aux [] lst;;

permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
