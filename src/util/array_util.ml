(* Copied an modified from ocaml-containers CCArray.ml *)
let bsearch ~cmp k a =
  let rec aux i j =
    if i > j then `Just_after j
    else
      let middle = i + ((j - i) / 2) in
      (* avoid overflow *)
      match cmp a.(middle) k with
      | 0 -> `At middle
      | n when n > 0 -> aux i (middle - 1)
      | _ -> aux (middle + 1) j
  in
  let n = Array.length a in
  if n = 0 then `Empty
  else
    match (cmp a.(0) k, cmp a.(n - 1) k) with
    | c, _ when c > 0 -> `All_bigger
    | _, c when c < 0 -> `All_lower
    | _ -> aux 0 (n - 1)
