(* 練習問題7.3 *)
exception Cannot_change
(* これで全探索できるのか、すごい *)
let rec change coins amount =
  match (coins, amount) with
      (_, 0) -> []
    | ((c :: rest) as coins, total) ->
        if c > total then change rest total
        else (
          try c :: change coins (total - c) with
            Cannot_change -> change rest total
        )
    | _ -> raise Cannot_change
  
let us_coins = [25; 10; 5; 1]
let gb_coins = [50; 20; 10; 5; 2; 1]

let check1 = change gb_coins 43
let check2 = change us_coins 43
let check3 = change [5; 2] 16
let check4 = change [5; 2] 41


