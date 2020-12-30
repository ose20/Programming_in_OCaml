(* 練習問題4.6 *)
let k x y = x
let s x y z = x z (y z)

let latter = k (s k k)

(*  
  latter x y = k (s k k) x y
              -> (s k k) y
              -> k y (k y)
              -> y 
*)