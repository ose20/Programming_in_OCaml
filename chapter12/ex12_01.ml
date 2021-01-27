(** 練習問題12.1 *)
(* 以下の calc の定義のおかしな点を指摘せよ。 *)
class calc =
  object
    val mutable num = 0
    val mutable func = fun x -> x
    
    method input n = num <- n
    method plus = func <- (fun y -> num + y)
    method eq = func num
  end

(*
  この定義だと、eq を呼び出した時の値は、
  num + num になってしまう。
  plus に出てくる num は、func が呼ばれた時の
  num の値と同じになる。
*)