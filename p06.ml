(* Palindrome *)
let rev lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] lst

let is_palindrome lst =
  lst = rev lst

let () = 
  assert (is_palindrome ["a";"n";"n";"a"] = true);
  assert (is_palindrome ["p";"a";"u";"l"] = false);
  assert (is_palindrome ["p"] = true);
  assert (is_palindrome [] = true);

