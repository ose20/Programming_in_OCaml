(* 練習問題3.3 *)
let andand2 b1 b2 = not (not b1 || not b2)

let oror2 b1 b2 = not (not b1 && not b2)

(* test *)
let test20 = andand2 true true = true
let test21 = andand2 true false = false
let test22 = andand2 false true = false
let test23 = andand2 false false = false
let test24 = oror2 true true = true
let test25 = oror2 true false = true
let test26 = oror2 false true = true
let test27 = oror2 false false = false