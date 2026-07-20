(* Stop at a breakpoint and print what each scope of the frame contains.

   The Global scope is the regression test for
   https://github.com/hackwaly/ocamlearlybird/issues/74: since OCaml 5.2 the
   SYMB section of a bytecode executable numbers globals by Symtable.Global.t
   rather than by Ident.t, and reading it with the old key type left the scope
   empty. *)

open Debug_protocol

let print_scope variables =
  variables
  |> List.iter (fun (variable : Variable.t) ->
         Printf.printf "  %s = %s\n" variable.name variable.value)

(* Which modules a program links in, and so what the Global scope lists, is up
   to the compiler; only the standard library is asserted on here. The point is
   that the scope is populated at all. *)
let print_global_scope variables =
  let names =
    variables |> List.map (fun (variable : Variable.t) -> variable.name)
  in
  Printf.printf "  includes Stdlib: %b\n" (List.mem "Stdlib" names)

let main () =
  Dap_client.with_session ~program:"fixtures/hello.bc"
    ~source:"fixtures/hello.ml" ~breakpoints:[ 12 ] (fun t ->
      let%lwt stopped = Dap_client.wait_stopped t in
      let thread_id = Option.value stopped.thread_id ~default:0 in
      let%lwt frame = Dap_client.top_frame t ~thread_id in
      Printf.printf "stopped: reason=%s at %s:%d\n"
        (Dap_client.string_of_reason stopped.reason)
        frame.name frame.line;
      let%lwt scopes = Dap_client.scopes t ~frame in
      scopes
      |> List.iter (fun (name, variables) ->
             Printf.printf "scope %s:\n" name;
             match name with
             | "Global" -> print_global_scope variables
             | _ -> print_scope variables);
      Lwt.return ())

let () = Lwt_main.run (main ())
