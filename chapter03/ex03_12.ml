let pos n =
  let rec iter (i, res) =
    if i < 0
    then res
    else iter (i - 1, res +. (1.0 /. (float_of_int (4 * i + 1))) -. (1.0 /. (float_of_int (4 * i + 3))))
  in
  iter (n, 0.0)