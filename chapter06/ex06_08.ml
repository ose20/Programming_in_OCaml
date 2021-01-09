(* 練習問題6.8 *)
exception Invalid_variant

type 'a tree =
    Lf
  | Br of 'a * 'a tree * 'a tree

type 'a rosetree =
    RLf
  | RBr of 'a * 'a rosetree list


let rec tree_of_rtree = function
    RLf -> Br(None, Lf, Lf)
  | RBr(a, rtrees) -> Br(Some a, tree_of_rtreelist rtrees, Lf)
and tree_of_rtreelist = function
    [] -> Lf
  | rtree :: rest -> 
      match tree_of_rtree rtree with
          Lf -> raise Invalid_variant
        | Br(a, left, Lf) -> Br(a, left, tree_of_rtreelist rest)
        | _ -> raise Invalid_variant

(*  
    tree_of_rtree の定義より、rtree の根と、下の兄弟がいない枝は
    この関数を適応すると必ず Br(x, left, Lf) というパターンに対応する。
    tree において右の子が Lf でないノードは rtree において、
    根でないかつ下に兄弟がいるノードである。
 *)

let rec rtree_of_tree = function
    Lf -> raise Invalid_variant
  | Br(Some x, left, Lf) -> RBr(x, rtreelist_of_tree left)
  | Br(None, left, Lf) -> RLf
  | Br(x, left, right) -> raise Invalid_variant 
and rtreelist_of_tree = function
    Lf -> []
  | Br(Some x, left, right) ->
      (* 
          tree_of_rtree を適用する前の rtree において根でないかつ下の兄弟がいる、
          すなわち tree において右の子が Lf でない場合にも、このように切り離してしまうので、
          先に定義した rtree_of_tree では右の子が Lf であるノードだけを扱えば良い
      *)
      let rtree = rtree_of_tree @@ Br(Some x, left, Lf) in
      rtree :: rtreelist_of_tree right
    (*  
        tree_of_rtree を適用する前は RLf だったものだけが None になるので、
        当然その RLf には子供がおらず、したがって tree_of_rtree を適用したあとの
        ノードでは左の子を持たないことになる
    *)
  | Br(None, Lf, right) -> RLf :: rtreelist_of_tree right
  | _ -> raise Invalid_variant

let rtree1 =
  RBr ("a", [
    RBr("b", [
      RBr("c", [RLf]);
      RLf;
      RBr("d", [RLf])
    ]);
    RBr("e", [RLf]);
    RBr("f", [RLf])
  ])

let rtree2 =
  RBr("a", [])

let rtree3 =
  RBr("a", [RBr("b", []); RLf])

let rtree4 =
  RBr("a", [
    RLf;
    RBr("b", [
      RBr("c", [RLf]);
      RLf;
      RBr("d", [])
    ])
  ])

let check1 = rtree_of_tree @@ tree_of_rtree rtree1 = rtree1
let check2 = rtree_of_tree @@ tree_of_rtree rtree2 = rtree2
let check3 = rtree_of_tree @@ tree_of_rtree rtree3 = rtree3
let check4 = rtree_of_tree @@ tree_of_rtree rtree4 = rtree4