(* Split a List Into Two Parts; The Length of the First Part Is Given *)
let split lst n =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | hd :: tl as l -> if i = 0 then List.rev acc, l
        else aux (i - 1) (hd :: acc) tl 
    in
    aux n [] lst


let () =
  assert (split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]));
  assert (split ["a";"b";"c"] 3 = (["a";"b";"c"], []));
  assert (split [] 0 = ([], []))
