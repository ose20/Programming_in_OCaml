(* 練習問題4.3 *)
let ($) f g x = f (g x)

let rec funny f n =
  if n = 0 then Fun.id
  else if n mod 2 = 0 then funny (f $ f) (n / 2)
  else funny (f $ f) (n / 2) $ f

(* funny f n x は x に f を n 回施した結果を返す *)