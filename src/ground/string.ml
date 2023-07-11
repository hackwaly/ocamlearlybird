open Stdlib
include String

let slice start end_ str = String.sub str start (end_ - start)

let cut_on_char sep s =
  match String.index s sep with
  | exception Not_found -> (s, "")
  | i -> (s |> slice 0 i, s |> slice (i + 1) (String.length s))
