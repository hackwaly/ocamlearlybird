(* Loading a file that is not an OCaml bytecode executable (e.g. a native
   executable, the mistake behind issue #33) should fail with a message that
   says so and points at the .bc, rather than "Bad magic". *)

open Debugger

let () =
  Lwt_main.run
    (try%lwt
       let%lwt _ = Bytecode.load_debuginfo "fixtures/not_bytecode.txt" in
       print_endline "expected loading to fail, but it succeeded";
       Lwt.return ()
     with exn ->
       print_endline (Printexc.to_string exn);
       Lwt.return ())
