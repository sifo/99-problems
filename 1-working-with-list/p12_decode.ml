
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let decode l =
  let rec aux acc = function
    | [] -> acc
    | (One x) :: t -> aux (x::acc) t
    | (Many (cnt, x)) :: t ->
      if (cnt-1) = 1
      then aux (x::acc) ((One x)::t)
      else aux (x::acc) ((Many (cnt-1, x))::t)
  in
  List.rev (aux [] l);; (* or `aux [] (List.rev list)` was good also *)

decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
