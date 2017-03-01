
let rec length = function
  | [] -> 0
  | _ :: tl -> 1 + (length tl);;

length [];;
length ['a'];;
length ['a'; 'b'];;

let length l =
  let rec call n = function
    | [] -> n
    | _ :: tl -> call (n+1) tl
  in
  call 0 l;;

length [];;
length ['a'];;
length ['a'; 'b'];;
