
let rec range i j =
  if i = j then
    [i]
  else if i < j then
    i :: (range (i+1) j)
  else
    i :: (range (i-1) j);;

range 4 9;;
range 9 4;;

let range i j =
  let rec aux acc curr =
    if curr = j then
      List.rev (curr::acc)
    else if curr < j then
      aux (curr::acc) (curr+1)
    else
      aux (curr::acc) (curr-1)
  in aux [] i;;


range 4 9;;
range 9 4;;
