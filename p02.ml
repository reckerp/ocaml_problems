(* Last Two Elements of a List *)

let rec last_two = function
  | [] -> None
  | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tl -> last_two tl

let () =
  assert (last_two [ 1; 2; 3 ] = Some (2, 3));
  assert (last_two [] = None);
  assert (last_two [ 1 ] = None)
