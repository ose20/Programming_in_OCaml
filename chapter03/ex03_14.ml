(* 練習問題3.14 *)
let integral f a b =
  let n = 10000 in (* 区間を 10000 等分する *)
  let delta = (b -. a) /. (float_of_int n) in
  let rec iter (i, res) =
    if i = 0 then res
    else 
      let s = (f(a +. delta *. float_of_int i) +. f(a +. delta *. float_of_int (i - 1))) *. delta /. 2.0 in
      iter (i - 1, res +. s)
  in iter (n, 0.0)


let pi = 3.141529
let ans = integral sin 0.0 pi