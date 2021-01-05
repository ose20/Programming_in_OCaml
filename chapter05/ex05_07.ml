(* 練習問題5.7 *)
(* int -> (int, int) list *)
let squares r =
  (*  0 <= y かつ y^2 <= r/2 が必要で、逆にこの条件下で求められた x も  *)
  (*  解の条件を満たすので、まずこの範囲で y を全探索し、x^2 = r - y^2   *)
  (*  を満たす x を二分探索で求めたい。時間計算量は O(\sqrt{r} \log r)  *)
  let rec iter y result =
    if y * y > r / 2 then result
    else
      let rec bin_search left right =
        if (left + 1 = right) && (left * left + y * y = r)
        then iter (y + 1) ((left, y) :: result)
        else if (left + 1 = right) && not (left * left + y * y = r)
        then iter (y + 1) result
        else
          let mid = (left + right) / 2 in
          if mid * mid <= r - y * y
          then bin_search mid right
          else bin_search left mid
      in
      bin_search 0 (r + 1)
  in
  iter 0 []
