
let drop list count =
  let rec aux acc curr = function
    | [] -> acc
    | h :: t ->
      if curr = 1 then
        aux acc count t
      else
        aux (h :: acc) (curr-1) t
  in List.rev (aux [] count list);;

drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 0;;
drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] (-1);;
drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 1;;

