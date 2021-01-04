(* 練習問題6.1 *)
type figure =
  | Point
  | Circle of int
  | Rectangle of int * int
  | Square of int

let similar f1 f2 =
  match (f1, f2) with
    | (Point, Point) | (Circle _ , Circle _) | (Square _, Square _) -> true
    | (Rectangle (x1, x2), Rectangle (x3, x4)) when x1 * x4 = x2 * x3 -> true
    | (Square _, Rectangle (x1, x2)) when x1 = x2 -> true
    | (Rectangle (x1, x2), Square _) when x1 = x2 -> true
    | _ -> false
