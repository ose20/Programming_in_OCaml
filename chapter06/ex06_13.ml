(* 練習問題6.13 *)
type intseq = Cons of int * (int -> intseq)

let rec next a b = Cons (a + b, next b)

let Cons (x1, f1) = Cons (1, next 0)
let Cons (x2, f2) = f1 x1
let Cons (x3, f3) = f2 x2
let Cons (x4, f4) = f3 x3
let Cons (x5, f5) = f4 x4
let Cons (x6, f6) = f5 x5
let Cons (x7, f7) = f6 x6
let Cons (x8, f8) = f7 x7

let fib = Cons(1, next 0)

let rec nthseq n (Cons(x, f)) =
  if n = 1 then x
  else nthseq (n - 1) (f x)