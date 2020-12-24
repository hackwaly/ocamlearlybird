type t = int

let make_alloc () =
  let next_id = ref 1 in
  fun () ->
    let id = !next_id in
    incr next_id;
    id
