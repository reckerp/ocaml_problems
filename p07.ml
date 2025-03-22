(* Flatten a List *)
type 'a node =
    | One of 'a 
    | Many of 'a node list

let flatten lst =
  let rec aux acc = function
    | [] -> acc
    | One hd :: tl -> aux (hd :: acc) tl  
    | Many hd :: tl -> aux (aux acc hd) tl  
  in
  List.rev (aux [] lst)

let () =
  assert(flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] = ["a"; "b"; "c"; "d"; "e"]);
  assert(flatten [] = [])



