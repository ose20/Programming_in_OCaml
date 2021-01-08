(* 練習問題6.7 *)
type 'a tree =
    Lf
  | Br of 'a * 'a tree * 'a tree

let rec reflect t =
  match t with
      Lf -> Lf
    | Br(x, left, right) -> Br(x, reflect right, reflect left)

let t1 =
  Br(1, Br(2, Br(4, Lf, Lf), Br(5, Lf, Lf)),
        Br(3, Br(6, Lf, Lf), Br(7, Lf, Lf)))

let check1 = reflect t1

let rec fold_left f e l =
  match l with
      [] -> e
    | x :: rest -> fold_left f (f e x) rest

let reverse l = fold_left (fun e x -> x :: e) [] l

(*
    preorder(reflect t) = reverse(postorder t) を t の構造に関する帰納法によって証明する。
    t = Lf のときは明らかに成り立つ
    t = Br(x, left, right) のとき
      まず帰納法の仮定より
        preorder(reflect left) = reverse(postorder left)
        preorder(reflect right) = reverse(postorder right)
      が成り立つ。このとき
      preorder(reflect Br(x, left, right))
        = preorder ( Br(x, reflect right, reflect left))
        = [x] @ preorder (reflect right) @ preorder (reflect left)
        = [x] @ reverse (postorder right) @ reverse (postorder left)
        = reverse (postorder left @ postorder right @ [x])
        = reverse (postorder Br(x, left, right))
      となり、確かに成り立つ。
    以上により証明が完了した。

    次に inorder (reflect t) = reverse (inorder t) を t の構造に関する帰納法で証明する。
    t = Lf のときは明らか
    t = Br(x, left, right) のとき
      帰納法の仮定より
        inorder (reflect left) = reverse (inorder left)
        inorder (reflect right) = reverse (reflect right)
      が成り立つ。このとき
      inorder (reflect Br(x, left, right))
        = inorder Br(x, reflect right, reflect left)
        = inorder (reflect right) @ [x] @ inorder (reflect left)
        = reverse (inorder right) @ [x] @ reverse (inorder left)
        = reveres (inorder left @ [x] @ inorder right)
        = reverse (inorder Br(x, left, right))
      となり、確かに成り立つ。
    以上により証明が完了した。

    次に、postorder (reflect t) = reverse (preorder t) を t の構造に関する帰納法で証明する。
    t = Lf のときは明らか
    t = Br(x, left, right) のとき
      帰納法の仮定より
        postorder (reflect left) = reverse (preorder left)
        postorder (reflect right) = reverse (preorder right)
      が成り立つ。このとき
      postorder (reflect Br(x, left, right))
        = postorder Br(x, reflect right, reflect left)
        = postorder (reflect right) @ postorder (reflect left) @ [x]
        = reverse (preorder right) @ reverse (preorder left) @ [x]
        = reverse ([x] @ preorder left @ preorder right)
        = reverse (preorder Br(x, left, right))
      となり、確かに成り立つ。
    以上により証明が完了した。
*)