
let encode l =
  let rec aux current acc = function
    | [] -> []
    | [x] -> (fst current + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b
      then aux ((fst current) + 1, a) acc t
      else aux (0, "") ((fst current + 1, a)::acc) t
  in List.rev (aux (0, "") [] l);;

let encode' l =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count+1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b
      then aux (count+1) acc t
      else aux 0 ((count+1, a)::acc) t
  in List.rev (aux 0 [] l);;

encode [];;
encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
encode ["a";"b";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;

encode' [];;
encode' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
encode' ["a";"b";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
