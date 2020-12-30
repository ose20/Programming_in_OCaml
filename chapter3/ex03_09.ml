(* 練習問題3.9 *)
let cond (b, e1, e2) : int = if b then e1 else e2
let rec fact n = cond ((n = 1), 1, n * fact (n - 1));;

fact 4 (* Stack overflow during evaluation (looping recursion?) *)

(* OCaml が値呼び戦略を採用していることによる
 * これにより，fact (n - 1) の値が求まるまで
 * cond (b, e1, e2) が if 式に変換されない
 * ところが fact n には終端条件がないので永遠に
 * 計算が終わらないことになる． *)