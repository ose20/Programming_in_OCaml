(** 練習問題8.9 *)
type 'a mlist =
    MNil
  | MCons of 'a * 'a mlist ref

type 'a queue = {mutable head : 'a mlist; mutable tail : 'a mlist}

let create () = {head = MNil; tail = MNil}

let add a qu =
  match qu with
      {head = MNil; tail = MNil} -> 
        let cell = MCons (a, ref MNil) in
        qu.head <- cell; qu.tail <- cell
    | {head = MCons (_, _); tail = MCons (b, nxt)} as tmp ->
        let cell = MCons (a, ref MNil) in
        nxt := cell; tmp.tail <- cell
    | _ -> failwith "enqueue: input queue is broken."

let peek = function
    {head = MNil; tail = MNil} -> None
  | {head = MCons(a, next); tail = MCons(_, _)} -> Some a
  | _ -> failwith "hd: input queue is broken."

let take = function
    {head = MCons(a, nxt); tail = MCons(_, _)} as q when q.head = q.tail ->
      q.head <- MNil; q.tail <- MNil; a
  | {head = MCons(a, nxt); tail = MCons(_, _)} as q ->
      q.head <- !nxt; a
  | {head = MNil; tail = MNil} -> failwith "dequeue: input queue is empty."
  | _ -> failwith "dequeue: input queue is broken."