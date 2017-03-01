
let slice l i k =
  let rec aux acc ci ck = function
    | [] -> acc
    | h :: t ->
      if ci = 0 && ck = 0 then
        acc
      else if ci = 0 && ck <> 0 then
        aux (h::acc) ci (ck-1) t
      else
        aux acc (ci-1) (ck-1) t
  in
  List.rev (aux [] i (k+1) l);;


slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 2;;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] (-2) (-2);;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 2;;

(* base on solution, but tail recursive *)

let slice list i k =
  let take n l =
    let rec aux acc n = function
      | [] -> acc
      | h :: t -> if n = 0 then acc else aux (h::acc) (n-1) t
    in List.rev (aux [] n l)
  in
  let rec drop n = function
    | [] -> []
    | h :: t as l -> if n = 0 then l else drop (n-1) t
  in
  take (k - i + 1) (drop i list);;

slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 2;;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] (-2) (-2);;
slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 2;;
