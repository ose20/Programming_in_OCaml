(* 練習問題6.4 *)
type nat = Zero | OneMoreThan of nat

let rec monus m n =
  match (m, n) with
      (Zero, Zero) -> Some Zero
    | (Zero, _) -> None
    | (m', Zero) -> Some m'
    | (OneMoreThan m', OneMoreThan n') -> monus m' n'

let zero = Zero
let one = OneMoreThan zero
let two = OneMoreThan one
let three = OneMoreThan two

let check0 = monus zero zero
let check1 = monus zero one
let check2 = monus zero two
let check3 = monus zero three
let check4 = monus one zero
let check5 = monus one two
let check6 = monus one three
let check7 = monus two zero
let check8 = monus two one
let check9 = monus two two
let check10 = monus two three
let check11 = monus three one
let check11 = monus three three