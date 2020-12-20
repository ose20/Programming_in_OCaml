(* 1 *)
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