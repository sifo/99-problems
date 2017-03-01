
let pack l =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> (
        match acc with
        | [] -> aux [[hd]] tl
        | hd2 :: tl2 -> (
            match hd2 with
            | [] -> aux ([[hd]] @ tl2) tl
            | hd3 :: _ ->
              if hd = hd3
              then aux ([hd::hd2] @ tl2) tl
              else aux ([[hd]; hd2] @ tl2) tl
          )
      )
  in List.rev (aux [] l);;

let pack' l =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl ->
      match acc with
      | [] -> aux [[hd]] tl
      | [] :: tl2 -> aux ([[hd]] @ tl2) tl
      | (hd3 :: _) as hd2 :: tl2 ->
        if hd = hd3
        then aux ([hd::hd2] @ tl2) tl
        else aux ([[hd]; hd2] @ tl2) tl
  in List.rev (aux [] l);;

let pack'' l =
  let rec aux acc l  = match (acc, l) with
    | _, [] -> acc
    | [], hd::tl -> aux [[hd]] tl
    | []::tl2, hd::tl -> aux ([[hd]] @ tl2) tl
    | (hd3 :: _) as hd2::tl2, hd::tl ->
      if hd = hd3
      then aux ([hd::hd2] @ tl2) tl
      else aux ([[hd]; hd2] @ tl2) tl
  in List.rev (aux [] l);;

let pack''' l =
  let rec aux current acc = function
    | [] -> []
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
      if a = b
      then aux (a :: current) acc t
      else aux [] ((a :: current) :: acc) t
  in List.rev (aux [] [] l);;


pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
pack' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
pack'' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
pack''' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;


(*

acc and l :
0. [] ["a", "a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
1. [["a"]] ["a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
2. [["a"; "a"]] ["a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
3. [["a"; "a"; "a"]] ["a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
4. [["a"; "a"; "a"; "a"]] ["b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
5. [["a"; "a"; "a"; "a"];["b"]] ["c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
5. [["a"; "a"; "a"; "a"];["b"];["c"]] ["c";"a";"a";"d";"d";"e";"e";"e";"e"]

etc.

*)
