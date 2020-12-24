(* 練習問題3.11 *)
(* 1 *)
let rec gcd m n =
  let m = min m n
  and n = max m n
  in
  if n mod m = 0 then m
  else gcd m (n mod m)

(* 2 *)
let rec comb (n, m) = (* ただし 0 <= m <= n *)
  if (n, m) = (n, 0) || (n, m) = (n, n) then 1
  else comb (n - 1, m) + comb (n - 1, m - 1)

(* 3 *)
let fib n =
  let rec iterfib (i, f1, f2) =(* i, F(i), F(i-1) *)
    if i = n then f1
    else iterfib (i + 1, f1 + f2, f1)
  in
  iterfib (1, 1, 0)

(* 4 *)
let max_ascii str =
  let n = String.length str in
  let rec iter (i, res) =
    if i = n then res
    else
      let code1 = Char.code (String.get str i) in
      let code2 = Char.code res in
      if code1 > code2
      then iter (i + 1, Char.chr code1)
      else iter (i + 1, Char.chr code2)
  in
  iter (1, String.get str 0)