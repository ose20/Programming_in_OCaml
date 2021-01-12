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


(* 別の方法 *)
type pairseq = PCons of int * int * (int -> int -> pairseq)

let rec nxt a b = PCons(b, a + b, nxt)
let PCons (y0, y1, g1) = PCons (0, 1, nxt)
let PCons (y1, y2, g2) = g1 y0 y1
let PCons (y2, y3, g3) = g2 y1 y2
let PCons (y3, y4, g4) = g3 y2 y3

let rec nthpairseq n (PCons(a, b, f)) =
  if n = 1 then b
  else nthpairseq (n - 1) (f a b)