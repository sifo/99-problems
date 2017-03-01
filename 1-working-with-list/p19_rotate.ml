
let rec rotate l n =
  if n = 0 then
    l
  else if n > 0 then
    match l with
    | [] -> []
    | h :: t -> rotate (List.rev(h::(List.rev t))) (n-1)
  else
    match (List.rev l) with
    | [] -> []
    | h :: t -> rotate (h::(List.rev t)) (n+1);;


rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
