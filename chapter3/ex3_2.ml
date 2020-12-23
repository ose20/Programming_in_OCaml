(* 練習問題3.2 *)
let andand b1 b2 =
  if b1 then b2 else false

let oror b1 b2 =
  if b1 then true else b2

(* test *)
let test12 = andand true true = true
let test13 = andand true false = false
let test14 = andand false true = false
let test15 = andand false false = false
let test16 = oror true true = true
let test17 = oror true false = true
let test18 = oror false true = true
let test19 = oror false false = false