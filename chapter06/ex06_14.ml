(* 練習問題6.14 *)
type intseq = Cons of int * (int -> intseq)

let time f =
  let start = Sys.time () in
  let res = f () in
  let end_ = Sys.time () in
  (res, end_ -. start)

(* 1 *)
let is_prime x =
  let rec iter i =
    if i >= x then true
    else if x mod i = 0 then false
    else iter (i + 1)
  in
  iter 2

let rec next_prime x =
  if is_prime (x + 1) then x + 1 else next_prime (x + 1)

let rec primeseq x =
  if is_prime (x + 1) then Cons (x + 1, primeseq) else primeseq (x + 1)

let rec nthseq n (Cons(x, f)) =
  if n = 1 then x else nthseq (n - 1) (f x)

let prime_seed = Cons(2, primeseq)

let check1 = time (fun () -> nthseq 5000 prime_seed)

(* 2 *)
let is_prime2 x =
  let upper_bound = int_of_float @@ floor @@ sqrt @@ float_of_int x in
  let rec iter i =
    if i > upper_bound then true
    else if x mod i = 0 then false
    else iter (i + 1)
  in
  iter 2

let rec primeseq2 x =
  if is_prime2 (x + 1) then Cons (x + 1, primeseq2) else primeseq2 (x + 1)

let prime_seed2 = Cons(2, primeseq2)

let check2 = time (fun () -> nthseq 5000 prime_seed2)

(* 3 *)
let rec is_prime3 primes x =
  match primes with
      [] -> true
    | prime :: rest ->
        if x mod prime = 0
        then false
        else is_prime3 rest x

let rec primeseq3 primes x =
  if is_prime3 primes (x + 1) 
  then Cons (x + 1, primeseq3 ((x + 1) :: primes))
  else primeseq3 primes (x + 1)

let prime_seed3 = primeseq3 [] 1

let check3 = time (fun () -> nthseq 5000 prime_seed3)

(* 4 *)
let rec is_prime4 primes x =
  let upper_bound = int_of_float @@ floor @@ sqrt @@ float_of_int x in
  match primes with
      [] -> true
    | prime :: rest when prime > upper_bound -> is_prime4 rest x
    | prime :: rest -> if x mod prime = 0 then false else is_prime4 rest x

let rec primeseq4 primes x =
  if is_prime4 primes (x + 1)
  then Cons (x + 1, primeseq4 ((x + 1) :: primes))
  else primeseq4 primes (x + 1)

let prime_seed4 = primeseq4 [] 1

let check4 = time (fun () -> nthseq 5000 prime_seed4)