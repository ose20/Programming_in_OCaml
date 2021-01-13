(* 練習問題6.15 *)
type ('a, 'b) sum =
    Left of 'a
  | Right of 'b

(* 'a * ('b * 'c) sum -> ('a * 'b, 'a * 'c) sum *)
let f1 (a, y) =
  match y with
      Left b -> Left (a, b)
    | Right c -> Right (a, c)

(* ('a * 'b, 'a * 'c) sum -> 'a * ('b, 'c) sum *)
let f2 ay =
  match ay with
      Left (a, b) -> (a, Left b)
    | Right (a, c) -> (a, Right c)

(* ('a, 'b) sum * ('c, 'd) sum -> (('a * 'c, 'b * 'c) sum, ('a * 'd, 'b * 'd) sum) sum *)
let f3 (x, y) =
  match (x, y) with
      (Left a, Left c) -> Left (Left (a, c))
    | (Left a, Right d) -> Right (Left (a, d))
    | (Right b, Left c) -> Left (Right (b, c))
    | (Right b, Right d) -> Right (Right (b, d))

(* (('a * 'c, 'b * 'c) sum, ('a * 'd, 'b * 'd) sum) sum -> ('a, 'b) sum * ('c, 'd) sum *)
let f4 xy =
  match xy with
      Left (Left (a, c)) -> (Left a, Left c)
    | Right (Right (b, d)) -> (Right b, Right d)
    | Right (Left (a, d)) -> (Left a, Right d)
    | Left (Right (b, c)) -> (Right b, Left c)

(* ('a -> 'b) * ('c -> 'b) -> ('a, 'c) sum -> 'b *)
let f5 (f, g) x =
  match x with
      Left a -> f a
    | Right c -> g c

(* (('a, 'b) sum -> 'c) -> ('a -> 'c) * ('b -> 'c) *)
let f6 f =
  let g a = f (Left a) in
  let h b = f (Right b) in
  (g, h)

(* ('a -> 'b, 'a -> 'c) sum -> 'a -> ('b, 'c) sum *)
let f7 = function
    Left f -> fun a -> Left (f a)
  | Right g -> fun a -> Right (g a)