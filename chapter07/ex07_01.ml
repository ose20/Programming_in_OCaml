(* 練習問題7.1 *)
let find x l =
  let rec find' = function
      [] -> raise Not_found
    | a :: rest when a = x -> 1
    | _ :: rest -> 1 + find' rest
  in
  try Some (find' l) with
      Not_found -> None

