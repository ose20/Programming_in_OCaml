(* 練習問題6.11 *)
type arith =
    Const of int
  | Add of arith * arith
  | Mul of arith * arith

let rec string_of_arith = function
    Const c -> string_of_int c
  | Add (Const x, Const y) -> (string_of_int x) ^ "+" ^ (string_of_int y)
  | Mul (Const x, Const y) -> (string_of_int x) ^ "*" ^ (string_of_int y)
  | Add (Mul (e1, e2), Mul (e3, e4)) -> (string_of_arith (Mul (e1, e2))) ^ "+" ^ (string_of_arith (Mul (e3, e4)))
  | Add (x, y) -> string_of_arith x ^ "+" ^ (string_of_arith y)
  | Mul (Const c, Mul(e1, e2)) -> string_of_arith (Const c) ^ "*" ^ (string_of_arith (Mul(e1, e2)))
  | Mul (Mul(e1, e2), Const c) -> string_of_arith (Mul(e1, e2)) ^ "*" ^ (string_of_arith (Const c))
  | Mul (Mul (e1, e2), Mul (e3, e4)) -> string_of_arith (Mul(e1, e2)) ^ "*" ^ (string_of_arith (Mul(e3, e4)))
  | Mul (x, y) -> "(" ^ string_of_arith x ^ ")" ^ "*" ^ "(" ^ (string_of_arith y) ^ ")"

let rec add_check = function
    Const c -> false
  | Add (a, b) -> true
  | Mul (a, b) -> add_check a || add_check b

let rec expand = function
    Const c -> Const c
  | Add (x, y) -> Add(expand x, expand y)
  | Mul (Const x, Const y) as whole -> whole
  | Mul (Add(a, b), c) -> Add (expand (Mul (a, c)), expand (Mul (b, c)))
  | Mul (a, Add(c, d)) -> Add (expand (Mul (a, c)), expand (Mul (a, d)))
  | Mul (a, b) when add_check (Mul (a, b)) ->  expand (Mul (expand a, expand b))
  | Mul (a, b) -> Mul (a, b)

let exp1 = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5)) (* (3+4) * (2+5) *)
let str1 = string_of_arith exp1
let check1 = string_of_arith (expand exp1)

let exp2 = Add(Mul(Add(Const 1, Const 2), Add(Const 3, Const 4)), Add(Const 5, Const 6)) (* ((1+2)*(3*4))+(5+6) *)
let str2 = string_of_arith exp2
let check2 = string_of_arith (expand exp2)

let exp3 = Mul(Mul(Add(Const 1, Const 2), Mul(Const 3, Const 4)), Add(Const 5, Const 6)) (* ((1+2)*(3*4))*(5+6) *)
let str3 = string_of_arith exp3
let check3 = string_of_arith (expand exp3)

let a = Mul(Add(Const 1, Const 2), Add(Const 3, Const 4))
let b = Mul(Add(Const 5, Const 6), Add(Const 7, Const 8))
let exp4 = Mul(a, b)
let str4 = string_of_arith exp4
let check4 = string_of_arith (expand exp4)

let exp5 = Mul(Add(Const 1, Const 2), Mul(Const 3, Const 4))
let str5 = string_of_arith exp5
let check5 = string_of_arith (expand exp5)

let exp6 = Mul(Const 1, Mul(Const 3, Const 4))
let str6 = string_of_arith exp6
