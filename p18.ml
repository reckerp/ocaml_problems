(* Extract a Slice From a List *)
let slice lst n m =
  let rec use n = function (* uses up to m, after first n was discarded *)
    | [] -> []
    | hd :: tl -> if n = 0 then [] else hd :: use (n - 1) tl
  in
  let rec disc n = function (* discards beginning until n *)
    | [] -> []
    | hd :: tl as l -> if n = 0 then l else disc (n - 1) tl
  in
  use (m - n+1) (disc n lst)

let () =
  assert (slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6 = ["c"; "d"; "e"; "f"; "g"]);
