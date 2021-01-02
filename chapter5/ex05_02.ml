(* 練習問題5.2 *)
(* 1 *)
let rec downto1 n =
  if n = 1 then [1] else n :: (downto1 (n - 1))

(* 末尾再帰的に書く *)
let downto1 n =
  let rec iter i rest =
    if i = n then n :: rest
    else iter (i + 1) (i :: rest)
  in
  iter 1 []


(* 2 *)
let roman_pair1 = [(1000, "M"); (500, "D"); (100, "C"); (50, "L");
      (10, "X"); (5, "V"); (1, "I")]

let roman_pair2 = [(1000, "M"); (900, "CM"); (500, "D"); (400, "CD");
      (100, "C"); (90, "XC"); (50, "L"); (40, "XL"); (10, "X");
      (9, "IX"); (5, "V"); (4, "IV"); (1, "I")]

let roman roman_pair_list n =
  let rec iter res n = function
    | [] -> res
    | (elt, str) :: rest ->
        if n / elt >= 2 then iter (res ^ str) (n - elt) ((elt, str) :: rest)
        else if n / elt >= 1 then iter (res ^ str) (n - elt) rest
        else iter res n rest
  in
  iter "" n roman_pair_list


(* 3 *)
let rec fold_right f l e =
  match l with
    [] -> e
  | a :: rest -> f a (fold_right f rest e)

let rec fold_left f e l =
  match l with
    [] -> e
  | a :: rest -> fold_left f (f e a) rest

let length list = fold_left (fun a _ -> a + 1) 0 list

let rec nested_length = function
    [] -> 0
  | x :: rest -> length x + (nested_length rest)

let check1 = nested_length [[[1;2;3]; [4;5;6]]; [[1;2;3]; [3;5;6]]]

(* 末尾再帰的に書くと *)
let nested_length list =
  let rec iter res = function
      [] -> res
    | x :: rest -> iter (length x + res) rest
  in
  iter 0 list


(* 4 *)
let rec concat list =
  match list with
    [] -> []
  | x :: rest -> x @ (concat rest)

(* 末尾再帰的 *)
let concat list =
  let rec iter res list =
    match list with
      [] -> res
    | x :: rest -> iter (res @ x) rest
  in
  iter [] list


(* 5 *)
let rec zip a_lst b_lst =
  match (a_lst, b_lst) with
    ([], _) | (_, []) -> []
  | (a :: a_rest, b :: b_rest) -> (a, b) :: (zip a_rest b_rest)

let check2 = zip [2; 3; 4; 5; 6; 7; 8; 9; 10; 11] 
  [true; true; false; true; false; true; false; false; false; true]


(* 6 *)
let unzip ab_lst =
  fold_right 
    (fun (a, b) (left, right) -> (a :: left, b :: right))   
    ab_lst
    ([], [])
  
let check3 = unzip check2


(* 7 *)
let filter p lst =
  fold_right
    (fun x res -> if p x then x :: res else res)
    lst
    []

let check4 = filter (fun x -> x > 0)
              [-9; 0; 2; 5; -3]

let check5 = filter (fun l -> length l = 3)
              [[1; 2; 3]; [4; 5]; [6; 7; 8]; [9]]


(* 8 *)
let take n lst =
  let rec iter i lst =
    match (i, lst) with
      (_, []) -> []
    | (i', _) when i' > n -> []
    | (i', x :: rest) -> x :: (iter (i + 1) rest)
  in
  iter 1 lst

let check6 = take 8 (downto1 10)

let drop n lst =
  let rec iter i lst =
    match (i, lst) with
      (_, []) -> []
    | (i', x :: rest) when i' <= n -> iter (i + 1) rest
    | (i', x :: rest) -> x :: (iter (i + 1) rest)
  in
  iter 1 lst

let check7 = drop 7 (downto1 10)


(* 9 *)
(* int list を仮定する *)
let max_lst lst = fold_left max min_int lst