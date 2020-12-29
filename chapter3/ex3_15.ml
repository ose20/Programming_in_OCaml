(* 練習問題3.15 *)

(* int -> int -> int -> int *)
(* int を 3 つ受け取って int を返す *)
let tri_sum a b c = a + b + c

(* (int -> int) -> int -> int *)
(* int -> int と int を受け取って int を返す *)
let apply f x = f (x + 1) + 1

(* (int -> int -> int) -> int *)
(* int -> int -> int を受け取って int を返す *)
let hoge f = f 2 7 + 1