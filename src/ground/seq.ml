open Stdlib
include Seq

let tap f t =
  Seq.map
    (fun it ->
      f it;
      it)
    t

let rec find_map_opt f seq =
  match seq () with
  | Seq.Nil -> None
  | Seq.Cons (hd, tl) -> (
      match f hd with Some r -> Some r | None -> find_map_opt f tl )

(* end_ is inclusive *)
let int_range ?(start = 0) ?end_ () =
  let i = ref start in
  let rec next () =
    let v = !i in
    match end_ with
    | Some end_ when v > end_ -> Nil
    | _ ->
        incr i;
        Seq.Cons (v, next)
  in
  next
