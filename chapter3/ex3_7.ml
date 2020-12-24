(* 練習問題3.7 *)

(* 1 *)
let pow1 (x, n) =
  let rec iter (i, res) =
    if i = n then res
    else iter (i + 1, res *. x)
  in
  iter (1, x)

(* 2 *)
let rec pow2 (x, n) =
  let rec iter (x, n, res) =
    if n = 0
    then res
    else if n mod 2 = 0
    then iter (x *. x, n/2, res)
    else iter (x, n - 1, res *. x)
  in
  iter (x, n, 1.0)