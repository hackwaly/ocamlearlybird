(* A breakpoint set in a source whose debug info uses dune's "/workspace_root"
   prefix should still bind. Since dune 3.0, map_workspace_root is on by default
   and rewrites the build directory in the debug info to a fixed
   "/workspace_root" that does not exist on disk; the adapter must map it back to
   the real directory (derived from the executable's location) to find the
   source. Without that, the source is never resolved, so no breakpoint in the
   module can bind and the program runs to completion. See the wsroot.bc rule in
   fixtures/dune. *)

open Debug_protocol

let main () =
  (* Line 3 is [let s = x + y in], inside Wsroot.add. *)
  Dap_client.with_session ~program:"fixtures/wsroot.bc"
    ~source:"fixtures/wsroot.ml" ~breakpoints:[ 3 ] (fun t ->
      let%lwt stopped = Dap_client.wait_stopped t in
      let thread_id = Option.value stopped.thread_id ~default:0 in
      let%lwt frame = Dap_client.top_frame t ~thread_id in
      Printf.printf "stopped at %s:%d\n" frame.name frame.line;
      let%lwt locals = Dap_client.scope t ~frame ~name:"Stack" in
      locals
      |> List.iter (fun (variable : Variable.t) ->
             Printf.printf "  %s = %s\n" variable.name variable.value);
      Lwt.return ())

let () = Lwt_main.run (main ())
