
let replicate list rep =
  if rep <= 0 then
    []
  else
    let rec aux acc curr = function
      | [] -> acc
      | (h :: t) as l ->
        if curr == 1 then
          aux (h :: acc) rep t
        else
          aux (h :: acc) (curr-1) l
    in List.rev (aux [] rep list);;

replicate ["a";"b";"c"] 3;;
replicate ["a";"b";"c"] 0;;
replicate ["a";"b";"c"] 1;;
replicate ["a"] 5;;
replicate [] 5;;
