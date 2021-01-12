(* 練習問題6.10 *)
type arith =
    Const of int
  | Add of arith * arith
  | Mul of arith * arith

let rec eval = function
    Const c -> c
  | Add (x, y) -> (eval x) + (eval y)
  | Mul (x, y) -> (eval x) * (eval y)

let exp = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5))
let check1 = eval exp