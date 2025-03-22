(* Pack Consecutive Duplicates *)
let pack lst = 
  let rec aux cur acc = function
    | [] -> []
    | [x] -> (x :: cur) :: acc
    | hd :: (hd' :: _ as tl) -> if hd = hd' then aux (hd :: cur) acc tl
       else aux [] ((hd :: cur) :: acc) tl
  in
  List.rev (aux [] [] lst)

let () = 
  assert (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"] = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
    ["e"; "e"; "e"; "e"]]);
  assert (pack ["a"] = [["a"]]);
  assert (pack [] = [])

