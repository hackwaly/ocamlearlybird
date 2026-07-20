(* Stop inside a function and walk the call stack: this covers the frames the
   adapter reports and the variables it finds in each of them. *)

open Debug_protocol

let main () =
  Dap_client.with_session ~program:"fixtures/hello.bc"
    ~source:"fixtures/hello.ml" ~breakpoints:[ 5 ] (fun t ->
      let%lwt stopped = Dap_client.wait_stopped t in
      let thread_id = Option.value stopped.thread_id ~default:0 in
      let%lwt frames = Dap_client.stack_trace t ~thread_id in
      frames
      |> Lwt_list.iter_s (fun (frame : Stack_frame.t) ->
             Printf.printf "frame %s at line %d\n" frame.name frame.line;
             let%lwt scopes = Dap_client.scopes t ~frame in
             scopes
             |> List.iter (fun (name, variables) ->
                    (* The Global scope is the same in every frame and has its
                       own test. *)
                    if name <> "Global" then
                      let variables =
                        variables
                        |> List.map (fun (variable : Variable.t) ->
                               Printf.sprintf "%s = %s" variable.name
                                 variable.value)
                      in
                      match variables with
                      | [] -> Printf.printf "  %s: <empty>\n" name
                      | variables ->
                          Printf.printf "  %s: %s\n" name
                            (String.concat ", " variables));
             Lwt.return ()))

let () = Lwt_main.run (main ())
