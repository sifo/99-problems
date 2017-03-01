
type 'a node =
  | One of 'a
  | Many of 'a node list;;

let rec flatten = function
  | [] -> []
  | hd :: tl ->
    match hd with
    | One e -> e :: flatten tl
    | Many l -> flatten l @ flatten tl;;

let flatten' l =
  let rec call acc = function
    | [] -> acc
    | hd :: tl ->
      match hd with
      | One e -> call (acc @ [e]) tl
      | Many l -> call (call acc l) tl
  in
  call [] l;;

let flatten'' l =
  let rec call acc = function
    | [] -> acc
    | One e :: tl -> call (e::acc) tl
    | Many l :: tl -> call (call acc l) tl
  in
  List.rev (call [] l);;

flatten [ ];;
flatten [ One "a" ];;
flatten [ Many [ One "b" ; Many [ One "c" ; One "d" ] ] ];;
flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;

flatten' [ ];;
flatten' [ One "a" ];;
flatten' [ Many [ One "b" ; Many [ One "c" ; One "d" ] ] ];;
flatten' [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;

flatten'' [ ];;
flatten'' [ One "a" ];;
flatten'' [ Many [ One "b" ; Many [ One "c" ; One "d" ] ] ];;
flatten'' [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;

