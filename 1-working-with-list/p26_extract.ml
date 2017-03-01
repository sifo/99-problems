
let extract_two l =
  let f e l =
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux ([e; h]::acc) t
    in aux [] l
  in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux ((f h t) @ acc) t
  in aux [] l;;


extract_two ["a";"b";"c";"d"];;

let rec extract n l =
  let f e l cur =
    let rec aux acc cur = function
      | [] -> acc
      | (h :: t) as l ->
        if cur = 2 then
          aux ([e; h]::acc) cur t
        else
          List.map (function x -> e :: x) (extract (cur-1) l)
    in aux [] cur l
  in
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux ((f h t n) @ acc) t
  in aux [] l;;


extract 2 ["a";"b";"c";"d"];;
extract 3 ["a";"b";"c";"d"];;
extract 4 ["a";"b";"c";"d"];;
extract 4 ["a";"b";"c";"d";"e";"f"];;
