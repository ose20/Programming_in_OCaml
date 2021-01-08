type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

let rec comptree x n =
  if n <= 0 then Lf
  else Br (x, comptree x (n - 1), comptree x (n - 1))

let comptree' n =
  let rec iter d x =
    if d <= 0 then Lf
    else Br (x, iter (d - 1) (2 * x), iter (d - 1) (2 * x + 1))
  in
  iter n 1