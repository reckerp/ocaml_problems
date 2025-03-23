let rec rm_nth n = function
  | [] -> []
  | hd :: tl -> if n = 0 then tl else hd :: rm_nth (n - 1) tl;;

let () =
  assert (rm_nth 1 ["a"; "b"; "c"; "d"] = ["a"; "c"; "d"])
