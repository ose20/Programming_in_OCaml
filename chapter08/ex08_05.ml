(* 練習問題8.5 *)
let x = ref []

(*
  このときの x の型は
  '_weak1 list ref
  になっている。ここで
*)

let _ = x := [1]

(*
  を試すと、x の型が
  int list ref
  になり、true :: !x
  で型エラーを出してくれるようになる
*)
