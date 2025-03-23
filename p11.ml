(* Modified Run-Length Encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode lst =
  let make_tpl c e =
    if c = 1 then One e else Many (c, e) 
  in

  let rec aux c acc = function
    | [] -> []
    | [x] -> (make_tpl (c + 1) x) :: acc
    | hd :: (hd' :: _ as tl) -> if hd = hd' then aux (c + 1) acc tl
       else aux 0 ((make_tpl (c + 1) hd) :: acc) tl in 
  List.rev (aux 0 [] lst)

let () =
  assert (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";Many (4, "e")]);
  assert (encode ["a"] = [One "a"]);
  assert (encode ["a";"a"] = [Many (2, "a")]);
  assert (encode [] = [])


