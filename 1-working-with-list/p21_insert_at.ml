
let rec insert_at e p = function
  | [] ->
    if p = 0 then
      [e]
    else
      []
  | (h :: t) as l ->
    if p = 0 then
      e :: l
    else
      h :: (insert_at e (p-1) t);;

insert_at "alfa" 1 ["a";"b";"c";"d"];;
insert_at "alfa" 3 ["a";"b";"c";"d"];;
insert_at "alfa" 4 ["a";"b";"c";"d"];;

let insert_at e p lst =
  let rec aux acc curr = function
    | [] ->
      if curr = 0 then
        List.rev (e::acc)
      else
        List.rev acc
    | (h :: t) as l ->
      if curr = 0 then
        List.rev (e::acc) @ l
      else
        aux (h::acc) (curr-1) t
  in aux [] p lst;;

insert_at "alfa" 1 ["a";"b";"c";"d"];;
insert_at "alfa" 3 ["a";"b";"c";"d"];;
insert_at "alfa" 4 ["a";"b";"c";"d"];;
