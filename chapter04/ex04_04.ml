(* 練習問題4.4 *)
let k x y = x
let s x y z = x z (y z)

(*  任意の term X について
    s k k X -> k X (k X)
              -> X 
    が成り立っている *)