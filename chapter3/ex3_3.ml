(* 問3.1.1 *)
let yen_of_dollar dollar =
  let rate = 114.32 in
  let base = dollar *. rate in
  (* 小数点第一位が5以上かどうかの判定 *)
  (* 2倍してから切り捨て *)
  let left = int_of_float (base *. 2.0) in
  (* 切り捨ててから2倍 *)
  let right = (int_of_float base) * 2 in
  (* left と right が異なれば切り上げが発生 *)
  if left = right then int_of_float base else int_of_float base + 1

(* test *)
let test1 = yen_of_dollar 1.0 = 114
let test2 = yen_of_dollar 2.0 = 229


(* 問3.1.2 *)
let dollar_of_yen yen =
  let rate = 114.32 in
  let base = (float_of_int yen) /. rate in 
  (* base の小数点第二位で四捨五入を行う *)
  (* 20倍して小数点以下切り捨て *)
  let left = int_of_float (base *. 20.0) in
  (* 10倍して小数点以下切り捨て2倍 *)
  let right = (int_of_float (base *. 10.)) * 2 in
  if (left = right)
  then floor (base *. 10.0) /. 10.0
  else floor (base *. 10.0) /. 10.0 +. 0.1

(* test *)
let test3 = dollar_of_yen 114 = 1.0 
let test4 = dollar_of_yen 8 = 0.1
let test5 = dollar_of_yen 110110 = 963.2


(* 問3.1.3 *)
let display_yen_of_dollar dollar =
  let yen = yen_of_dollar dollar in
  (string_of_float dollar) ^ " dollars are " ^ (string_of_int yen) ^ " yen."

(* test *)
let test6 = display_yen_of_dollar 1.0 = "1. dollars are 114 yen."


(* 問3.1.4 *)
let capitalize c =
  if 97 <= Char.code c && Char.code c <= 122
  then Char.chr (Char.code c - 32)
  else c

(* test *)
let test7 = capitalize 'a' = 'A'
let test8 = capitalize 'A' = 'A'
let test9 = capitalize 'z' = 'Z'
let test10 = capitalize '3' = '3'
let test11 = capitalize '#' = '#'


(* 問3.2 *)
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


(* 問3.3 *)
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