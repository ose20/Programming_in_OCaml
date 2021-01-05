(* 練習問題6.2 *)
exception Invalid_variant

type figure =
  | Point
  | Circle of int
  | Rectangle of int * int
  | Square of int

type 'a with_location = {loc_x : float; loc_y : float; body : 'a}

let sq_dist (x1, y1) (x2, y2) = (x2 -. x1) *. (x2 -. x1) +. (y2 -. y1) *. (y2 -. y1)

let rectangle_of_square f =
  match f.body with
    | Square x -> {f with body = Rectangle (x, x)}
    | _ -> raise Invalid_variant

let overlap_point_point f1 f2 =
  match (f1, f2) with 
      ({loc_x = x1; loc_y = y1; body = Point}, {loc_x = x2; loc_y = y2; body = Point})
        -> x1 = x2 && y1 = y2
    | _ -> raise Invalid_variant

let overlap_point_circle f1 f2 =
  match (f1, f2) with
      ({loc_x = x1; loc_y = y1; body = Point}, {loc_x = x2; loc_y = y2; body = Circle r})
        -> sq_dist (x1, y1) (x2, y2) <= float_of_int (r * r)
    | _ -> raise Invalid_variant

let overlap_point_rectangle f1 f2 =
  match (f1, f2) with
      ({loc_x = x1; loc_y = y1; body = Point}, {loc_x = x2; loc_y = y2; body = Rectangle (l1, l2)})
        ->    x2 -. (float_of_int l1 /. 2.0) <= x1
          &&  x1 <= x2 +. (float_of_int l1 /. 2.0)
          &&  y2 -. (float_of_int l2 /. 2.0) <= y1
          &&  y1 <= y2 +. (float_of_int l2 /. 2.0)
    | _ -> raise Invalid_variant

let overlap_circle_circle f1 f2 =
  match (f1, f2) with
      ({loc_x = x1; loc_y = y1; body = Circle r1}, {loc_x = x2; loc_y = y2; body = Circle r2})
        -> sq_dist (x1, y1) (x2, y2) <= float_of_int @@ (r1 + r2) * (r1 + r2)
    | _ -> raise Invalid_variant

let overlap_circle_rectangle f1 f2 =
  match (f1, f2) with
      ({loc_x = x1; loc_y = y1; body = Circle r1}, {loc_x = x2; loc_y = y2; body = Rectangle (a2, b2)})
        ->  let left2 = x2 -. (float_of_int a2 /. 2.0) in
            let right2 = x2 +. (float_of_int a2 /. 2.0) in
            let bottom2 = y2 -. (float_of_int b2 /. 2.0) in
            let upper2 = y2 +. (float_of_int b2 /. 2.0) in
            (* 長方形の左上から反時計回りに順番に場合わけ *)
            if x1 <= left2 && upper2 <= y1
            then sq_dist (left2, upper2) (x1, y1) <= float_of_int @@ r1 * r1
            else if x1 <= left2 && bottom2 <= y1 && y1 <= upper2
            then left2 -. x1 <= float_of_int r1
            else if x1 <= left2 && y1 <= bottom2
            then sq_dist (left2, bottom2) (x1, y1) <= float_of_int @@ r1 * r1
            else if left2 <= x1 && x1 <= right2 && y1 <= bottom2
            then bottom2 -. y1 <= float_of_int r1
            else if right2 <= x1 && y1 <= bottom2
            then sq_dist (x1, y1) (right2, bottom2) <= float_of_int @@ r1 * r1
            else if right2 <= x1 && bottom2 <= y1 && y1 <= upper2
            then x2 -. right2 <= float_of_int r1
            else if right2 <= x1 && upper2 <= y1
            then sq_dist (x1, y1) (right2, upper2) <= float_of_int @@ r1 * r1
            else if left2 <= x1 && x1 <= right2 && upper2 <= y1
            then y1 -. upper2 <= float_of_int r1
            else true
    | _ -> raise Invalid_variant

let overlap_rectangle_rectangle f1 f2 =
  match (f1, f2) with
      ({loc_x = x1; loc_y = y1; body = Rectangle (a1, b1)}, {loc_x = x2; loc_y = y2; body = Rectangle (a2, b2)})
        ->  let left1 = x1 -. (float_of_int a1 /. 2.0) in
            let right1 = x1 +. (float_of_int a1 /. 2.0) in
            let bottom1 = y1 -. (float_of_int b1 /. 2.0) in
            let upper1 = y1 +. (float_of_int b1 /. 2.0) in
            let left2 = x2 -. (float_of_int a2 /. 2.0) in
            let right2 = x2 +. (float_of_int a2 /. 2.0) in
            let bottom2 = y2 -. (float_of_int b2 /. 2.0) in
            let upper2 = y2 +. (float_of_int b2 /. 2.0) in
            left2 <= right1 && left1 <= right2 && bottom1 <= upper2 && bottom2 <= upper1
    | _ -> raise Invalid_variant

let overlap f1 f2 =
  match f1.body with
      Point -> 
        (match f2.body with
            Point -> overlap_point_point f1 f2
          | Circle _ -> overlap_point_circle f1 f2
          | Rectangle (_, _) -> overlap_point_rectangle f1 f2
          | Square _ -> overlap_point_rectangle f1 (rectangle_of_square f2)
        )
    | Circle _ ->
        (match f2.body with
            Point -> overlap_point_circle f2 f1
          | Circle _ -> overlap_circle_circle f1 f2
          | Rectangle (_, _) -> overlap_circle_rectangle f1 f2
          | Square _ -> overlap_circle_rectangle f1 (rectangle_of_square f2)
        )
    | Rectangle (_, _) ->
        (match f2.body with
            Point -> overlap_point_rectangle f2 f1
          | Circle _ -> overlap_circle_rectangle f2 f1
          | Rectangle (_, _) -> overlap_rectangle_rectangle f1 f2
          | Square _ -> overlap_rectangle_rectangle f1 (rectangle_of_square f2)
        )
    | Square _ ->
        (match f2.body with
            Point -> overlap_point_rectangle f2 (rectangle_of_square f1)
          | Circle _ -> overlap_circle_rectangle f2 (rectangle_of_square f1)
          | Rectangle (_, _) -> overlap_rectangle_rectangle (rectangle_of_square f1) f2
          | Square _ -> overlap_rectangle_rectangle (rectangle_of_square f1) (rectangle_of_square f2)
        )