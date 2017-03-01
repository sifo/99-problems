
let rec lotto_select n b =
  if n <= 0
  then []
  else (Random.int b) :: (lotto_select (n-1) b);;

lotto_select 6 49;;

let lotto_select n b =
  let rec aux acc cur =
    if cur <= 0 then
      acc
    else
      aux ((Random.int b)::acc) (cur-1)
  in aux [] n;;

lotto_select 6 49;;
