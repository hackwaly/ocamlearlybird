type t = int

let make_alloc next_id =
  let next_id = ref next_id in
  fun () ->
    let id = !next_id in
    incr next_id;
    id
