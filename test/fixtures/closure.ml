(* Fixture for the Heap scope test: [bump] captures [count] and [step] from its
   enclosing scope, so they live in the closure's environment (the Heap scope)
   rather than on the stack. Keep the line numbers stable as the tests set
   breakpoints by line. *)

let make_counter start step =
  let count = ref start in
  let bump () =
    let bumped = !count + step in
    count := bumped;
    bumped
  in
  bump

let () =
  let bump = make_counter 10 5 in
  let first = bump () in
  let second = bump () in
  print_endline (string_of_int (first + second))
