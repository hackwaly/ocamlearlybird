type t = int

let make_alloc () =
  let counter = ref 0 in
  fun () ->
    let id = !counter in
    incr counter;
    id
