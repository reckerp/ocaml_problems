(* Eliminate consecutive Duplicates *)
let rec rm_consecutives = function
  | hd:: (hd' :: _ as tl) -> if hd = hd' then rm_consecutives tl else hd :: rm_consecutives tl
  | a -> a

let () =
    assert (rm_consecutives ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] = ["a"; "b"; "c"; "a"; "d"; "e"]);
    assert (rm_consecutives ["a";"b";"c"] = ["a";"b";"c"]);
    assert (rm_consecutives [] = [])

