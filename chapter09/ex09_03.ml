(** 練習問題9.3 *)
(* 以下のシグネチャを持つモジュールを実装する *)

module type QUEUE =
  sig
    type 'a t
    exception Empty
    val empty: 'a t
    val add: 'a t -> 'a -> 'a t
    val take: 'a t -> 'a * 'a t
    val peek: 'a t -> 'a
  end

(* Queue1 はキューをリストで表現する *)
module Queue1 : QUEUE =
  struct
    type 'a t = 'a list
    exception Empty
    let empty = []

    (* add で O(n) かかる *)
    let rec add lst a =
      match lst with
          [] -> [a]
        | x :: rest -> x :: add rest a

    let take = function
        [] -> raise Empty
      | x :: rest -> (x, rest)

    let peek = function
        [] -> raise Empty
      | x :: rest -> x
  end

(* Queue2 では、キューをリストのペアでもつ *)
module Queue2 : QUEUE =
  struct
    type 'a t = Queue of ('a list * 'a list)
    exception Empty
    let empty = Queue ([], [])

    let add q a =
      match q with
          Queue ([], []) -> Queue ([a], [])
        | Queue (left, right) -> Queue (left, a :: right)

    let reverse lst =
      let rec iter res = function
          [] -> res
        | x :: rest -> iter (x :: res) rest
      in iter [] lst

    (* 取り出した後に左のリストが空になってしまうときだけ、右のリストを反転して左に移す *)
    let take = function
        Queue (x :: rest, right) when not (rest = []) -> (x, Queue (rest, right))
      | Queue (x :: rest, right) -> (x, Queue (reverse right, []))
      | _ -> raise Empty


    let peek = function
        Queue ([], _) -> raise Empty
      | Queue (x :: _, _) -> x
  end

let q1 = Queue1.empty
let q1 = (Queue1.add (Queue1.add (Queue1.add (Queue1.add q1 1) 2) 3) 4)
let x = Queue1.peek q1
let (x, q1) = Queue1.take q1
let (x, q1) = Queue1.take q1
let q1 = Queue1.add (Queue1.add q1 5) 6
let (x, q1) = Queue1.take q1
let (x, q1) = Queue1.take q1
let (x, q1) = Queue1.take q1
let (x, q1) = Queue1.take q1

let q2 = Queue2.empty
let q2 = (Queue2.add (Queue2.add (Queue2.add (Queue2.add q2 1) 2) 3) 4)
let x = Queue2.peek q2
let (x, q2) = Queue2.take q2
let (x, q2) = Queue2.take q2
let q2 = Queue2.add (Queue2.add q2 5) 6
let (x, q2) = Queue2.take q2
let (x, q2) = Queue2.take q2
let (x, q2) = Queue2.take q2
let (x, q2) = Queue2.take q2