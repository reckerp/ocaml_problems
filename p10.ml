(* Run-Length Encoding *)
let pack lst = 
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count + 1, x) :: acc
    | hd :: (hd' :: _ as tl) -> if hd = hd' then aux (count + 1) acc tl
       else aux 0 ((count + 1, hd) :: acc) tl
  in
  List.rev (aux 0 [] lst)

let () = 
  assert (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);
  assert (pack ["a"] = [(1, "a")]);
  assert (pack [] = [])

