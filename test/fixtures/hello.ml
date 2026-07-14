(* Fixture program debugged by the integration tests. Keep the line numbers
   stable as the tests set breakpoints by line. *)

let greet name =
  let greeting = "Hello, " ^ name in
  greeting

let () =
  let x = 41 in
  let y = x + 1 in
  let msg = greet "world" in
  print_endline msg;
  print_endline (string_of_int y)
