(* 練習問題6.3 *)
type nat = Zero | OneMoreThan of nat

let rec add m n =
  match m with
      Zero -> n
    | OneMoreThan m' -> OneMoreThan (add m' n)

let rec nat m n =
  match m with
      Zero -> Zero
    | OneMoreThan m' -> add n (nat m' n)

let rec monus m n =
  match (m, n) with
      (Zero, n') -> Zero
    | (m', Zero) -> m'
    | (OneMoreThan m', OneMoreThan n') -> monus m' n'

let zero = Zero
let one = OneMoreThan zero
let two = OneMoreThan one
let three = OneMoreThan two

let check1 = monus one zero
let check2 = monus one one
let check3 = monus one two
let check4 = monus one three
let check5 = monus three zero
let check6 = monus three two
let check7 = monus two three