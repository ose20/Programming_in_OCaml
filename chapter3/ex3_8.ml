(* 練習問題3.8 *)
let iterpow (x, n) =
  let rec iter (i, res) =
    if i = n then res
    else iter (i + 1, res *. x)
  in
  iter (0, 1.0)