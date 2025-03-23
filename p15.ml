(* Replicate the Elements of a List a Given Number of Times *)
let replicate lst n =
  let rec add n acc elem =
    if n = 0 then acc else add (n-1) (elem :: acc) elem in
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (add n acc hd) tl 
  in
  List.rev (aux [] lst)

let () =
  assert (replicate [1;2;3;4;5] 2 = [1;1;2;2;3;3;4;4;5;5]);
  assert (replicate [1;2;3;4;5] 0 = []);
  assert (replicate [1;2;3;4;5] 1 = [1;2;3;4;5]);
  assert (replicate [] 0 = []);
