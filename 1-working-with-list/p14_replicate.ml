
let duplicate l =
  let rec aux acc  = function
    | [] -> acc
    | h :: t -> aux (h :: h :: acc) t
  in List.rev (aux [] l);;

duplicate [];;
duplicate ["a"];;
duplicate ["a"; "b"; "c"; "c"; "d"];;
