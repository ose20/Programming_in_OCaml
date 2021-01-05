(* 練習問題4.2 *)
let rec repeat f n x =
  if n = 0 then x
  else repeat f (n - 1) (f x)

let fib n =
  let f (a, b) = (a + b, a) in
  let (fibn, _) = repeat f (n - 1) (1, 0)
  in fibn