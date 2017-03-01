
let split list count =
  let rec aux (a1, a2) curr = function
    | [] -> (List.rev a1, List.rev a2)
    | h :: t ->
      if curr = 0 then
        aux (a1, h::a2) curr t
      else
        aux (h::a1, a2) (curr-1) t
  in aux ([], []) count list;;

let split list count =
  let rec aux (a1, a2) curr = function
    | [] -> (List.rev a1, [])
    | h :: t ->
      if curr = 0 then
        (List.rev a1, t)
      else
        aux (h::a1, a2) (curr-1) t
  in aux ([], []) count list;;

split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
split ["a";"b";"c";"d"] 5;;
split ["a";"b";"c";"d"; "e"] 5;;
