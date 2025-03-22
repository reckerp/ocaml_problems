(* N'th Element of a List *)

let rec nth n = function
  | [] -> None
  | hd :: tl -> if n = 0 then Some hd else nth (n - 1) tl

let () = 
  assert (nth 0 [1;2;3] = Some 1);
  assert (nth 1 [1;2;3] = Some 2);
  assert (nth 2 [] = None)
