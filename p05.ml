(* Reverse a List *)
let rev lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] lst

let () =
  assert (rev [1; 2; 3] = [3; 2; 1]);
  assert (rev [1] = [1]);
  assert (rev [] = []);
