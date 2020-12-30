(* 練習問題4.1 *)
let uncurry f (x, y) = f x y

let curried_avg x y = (x +. y) /. 2.0
let avg = uncurry curried_avg

let test = avg (4.0, 5.3)