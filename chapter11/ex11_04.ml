(** 練習問題11.4 *)
(*
  NaiveSig が与えられていない生の BadPair がファンクター MakeTest
  に渡せないことを確かめよ。  
*)
module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module type SET =
  sig
    type elt
    type t

    val empty : t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val inter : t -> t -> t
    val elements : t -> elt list
  end

module MakeAbstractSet (Order : OrderedType) :
  SET with type elt = Order.t
=
  struct
    type elt  = Order.t
    type t = elt list

    let empty = []

    let rec mem elt = function
      | [] -> false
      | x :: rest ->
          match Order.compare elt x with
          | 0 -> true
          | r when r < 0 -> false
          | _ -> mem elt rest
    
    let rec add elt = function
      | [] -> [elt]
      | (x :: rest as s) ->
          match Order.compare elt x with
          | 0 -> s
          | r when r < 0 -> elt :: s
          | _ -> x :: (add elt rest)

    let rec inter s1 s2 =
      match (s1, s2) with
      | ([], _) -> []
      | (_, []) -> []
      | ((e1 :: rest1 as s1), (e2 :: rest2 as s2)) ->
          match Order.compare e1 e2 with
          | 0 -> e1 :: (inter rest1 rest2)
          | r when r < 0 -> inter rest1 s2
          | _ -> inter s1 rest2
      
    let elements s = s
  end

module Pair =
  struct
    module Elt =
      struct
        type t = int
        let compare i j = i - j
      end
    module Set = MakeAbstractSet (Elt)
  end

module BadPair =
  struct
    module Elt =
      struct
        type t = int
        let compare i j = i -j
      end
    module Set = MakeAbstractSet (
      struct
        type t = string
        let compare s1 s2 = String.length s1 - String.length s2
      end
    )
  end

module type Psig =
  sig
    module Elt : OrderedType
    module Set : SET with type elt = Elt.t
  end

module MakeTest (P : Psig) =
  struct
    let test_elements set =
      let rec loop = function
        | [] | [_] -> true
        | x :: y :: rest ->
          if P.Elt.compare x y > 0 then false
          else loop (y :: rest)
    in loop (P.Set.elements set)
  end

(*
  module BadTest = MakeTest (BadPair)
  Error: Signature mismatch:
        Modules do not match:
        sig module Elt = BadPair.Elt module Set = BadPair.Set end
        is not included in
        NaiveSig
        ...
        In module Set:
        Type declarations do not match:
        type elt = string
        is not included in
        type elt = Elt.t
*)
