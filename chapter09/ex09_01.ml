(** 練習問題9.1 *)
(* Zarith を使用 cf https://github.com/ocaml/Zarith *)

#require "zarith"

(* deriv f : (Q.t -> Q.t) -> Q.t -> Q.t *)
let deriv f =
  let dx = Q.of_ints 1 (int_of_float 1e11) in
  fun x -> Q.div (Q.sub (f (Q.add x dx)) (f x)) dx

(* a の本来の値は 27.0 なので、計算してみるとこちらの方が精度が高いことがわかる *)
let a = deriv (fun q -> Q.mul (Q.mul q q) q) (Q.of_ints 3 1)
let _ = Q.print a

let fixpoint f init = (* f の不動点を(誤差を 1e-11 として)求める *)
  let threshold = Q.of_ints 1 (int_of_float 1e11) in
  let rec loop x =
    let next = f x in
    if Q.lt (Q.abs (Q.sub x next)) threshold
    then x
    else loop next
  in loop init

(* 元の関数から、不動点を求める関数に変形する *)
let newton_transform f =
  fun x -> Q.sub x (Q.div (f x) (deriv f x))

(* 初期値 guess からニュートン法を使う *)
let newton_method f guess = fixpoint (newton_transform f) guess

let square_root a = newton_method (fun x -> Q.sub (Q.mul x x) a) (Q.of_ints 1 1)
let sqrt5 = Q.to_float @@ square_root (Q.of_ints 5 1)