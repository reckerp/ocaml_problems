(* Length of a List *)

let len lst = 
  let rec aux acc = function
    | [] -> acc
    | _ :: tl -> aux (acc + 1) tl
  in
  aux 0 lst

let () =
  assert (len [1; 2; 3] = 3);
  assert (len [] = 0);
