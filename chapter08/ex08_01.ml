(* 練習問題8.1 *)
type 'a cell = {mutable contents : 'a}

(* 'a -> 'a ref *)
let ref a = {contents = a}
let ( ! ) {contents = a} = a
let ( := ) c a = c.contents <- a