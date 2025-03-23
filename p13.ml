(* Run-Length Encoding of a List (Direct Solution) *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode list =
    let make_rle c elem = if c = 0 then One elem else Many (c + 1, elem) in
    let rec aux c acc = function
      | [] -> []
      | [x] -> make_rle c x :: acc
      | a :: (b :: _ as t) -> if a = b then aux (c + 1) acc t
                              else aux 0 (make_rle c a :: acc) t
    in
      List.rev (aux 0 [] list)

let () =
  assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
    Many (4, "e")]);
  assert (encode ["a"] = [One "a"]);
  assert (encode ["a";"a"] = [Many (2, "a")]);
  assert (encode [] = [])


