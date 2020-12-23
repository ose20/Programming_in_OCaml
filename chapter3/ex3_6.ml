(* 練習問題3.6 *)
(* 1 *)
let geo_mean (x, y) = sqrt (x *. y)

(* 2 *)
let bmi (name, height, weight) =
  let bmi_idx = weight /. (height *. height) in
  let health = 
    if bmi_idx < 18.5 then "やせ"
    else if bmi_idx < 25.0 then "標準"
    else if bmi_idx < 30.0 then "肥満"
    else "高度肥満" in
  name ^ "さんの健康状態は" ^ "「" ^ health ^ "」です"

(* 3 *)
let f (x, y) = ((x + y) / 2, (x - y) / 2)
