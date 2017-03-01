
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

let rand_select lst n =
  let rec aux acc curr = function
    | [] ->
      acc
    | l ->
      if curr = 0 then
        acc
      else
        let i = Random.int (List.length l) in
        let e = match (at (i+1) l) with Some(x) -> x | None -> assert false in
        aux (e::acc) (curr-1) (remove_at i l)
  in aux [] n lst;;

rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
rand_select ["a";"b";"c"] 3;;
rand_select ["a";"b"] 3;;
