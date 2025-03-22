(* Tail of a list *)

let rec last = function [] -> None | [ x ] -> Some x | _ :: t -> last t

let () =
  assert (last [ 1; 2; 3; 4; 5 ] = Some 5);
  assert (last [] = None)
