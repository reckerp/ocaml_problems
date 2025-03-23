(* Duplicate the Elements of a List *)

let rec dup = function
  | [] -> []
  | hd :: tl -> hd :: hd :: (dup tl)

let () =
  assert (dup [1;2;3;4;5] = [1;1;2;2;3;3;4;4;5;5]);
  assert (dup [1] = [1;1]);
  assert (dup [] = [])
