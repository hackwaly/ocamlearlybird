open Stdlib
include Array

module Sorted = struct
  let bsearch ~cmp k a =
    Algorithms.binary_search ~cmp ~get:Array.get a 0 (Array.length a) k

  let slice_bs ~cmp start end_ a =
    let start =
      match a |> bsearch ~cmp start with
      | `Empty -> 0
      | `At i -> i
      | `Just_after i -> i + 1
    in
    let end_ =
      match a |> bsearch ~cmp end_ with
      | `Empty -> 0
      | `At i -> i
      | `Just_after i -> i + 1
    in
    Array.sub a start (end_ - start)
end
