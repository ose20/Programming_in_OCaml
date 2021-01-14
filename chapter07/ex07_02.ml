(* 練習問題7.2 *)
exception Find_zero
let prod_list l =
  let rec prod_list' res = function
      [] -> res
    | 0 :: rest -> raise Find_zero
    | x :: rest -> prod_list' (res * x) rest
  in
  try prod_list' 1 l with
      Find_zero -> 0