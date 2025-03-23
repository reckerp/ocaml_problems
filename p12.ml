(* Decode a Run-Length Encoded List *)
type 'a rle =
  | One of 'a
  | Many of int * 'a


let decode lst = 
  let rec listify_many c acc elem =
    if c = 0 then acc else listify_many (c-1) (elem :: acc) elem 
  in

  let rec aux acc = function
    | [] -> acc
    | One hd :: tl -> aux (hd :: acc) tl
    | Many (n, x) :: tl -> aux (listify_many n acc x) tl
  in
  aux [] (List.rev lst)

let () =
  assert (decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);
  assert (decode [One "a"] = ["a"]);
  assert (decode [Many (2, "a")] = ["a"; "a"]);
  assert (decode [] = [])
