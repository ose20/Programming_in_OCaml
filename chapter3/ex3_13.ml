(* 練習問題3.13 *)
(* 指数が第1引数になっている pow 関数 *)
let pow n x =
  let rec iter (i, res) =
    if i = n then res
    else iter (i + 1, res *. x)
  in iter (0, 1.0)

let cube = pow 3 

(* 指数が第2引数になっている pow 関数 *)
let pow2 x n =
  let rec iter (i, res) =
    if i = n then res
    else iter (i + 1, res *. x)
  in iter (0, 1.0)

let cube2 x = pow2 x 3