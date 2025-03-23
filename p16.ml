(* Drop Every N'th Element From a List *)
let drop lst n =
  let rec aux i = function
    | [] -> [] 
    | hd :: tl -> if i = n then aux 1 tl else hd :: (aux (i+1) tl)
  in
  aux 1 lst

let () = 
  assert (drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]);
  assert (drop [] 0 = [])
