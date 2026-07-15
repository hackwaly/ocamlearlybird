(* Walk through the fixture with the stepping commands and record where the
   debuggee comes to rest each time.

   The interesting, stable signal is the shape of the walk: [next] stays in the
   current function, [step in] descends into the callee (one more frame), [step
   out] returns to the caller (one fewer). Exact landing lines are printed too,
   but those are the compiler's to decide. *)

open Debug_protocol

let report t ~thread_id label =
  let%lwt frames = Dap_client.stack_trace t ~thread_id in
  let depth = List.length frames in
  let top = List.hd frames in
  Printf.printf "%-9s -> %s:%d  (depth %d)\n" label top.Stack_frame.name
    top.line depth;
  Lwt.return ()

let main () =
  (* Line 10 is [let y = x + 1 in]; line 11 the call to [greet]. *)
  Dap_client.with_session ~program:"fixtures/hello.bc"
    ~source:"fixtures/hello.ml" ~breakpoints:[ 10 ] (fun t ->
      let%lwt stopped = Dap_client.wait_stopped t in
      let thread_id = Option.value stopped.thread_id ~default:0 in
      let%lwt () = report t ~thread_id "breakpoint" in
      let%lwt _ = Dap_client.next t ~thread_id in
      let%lwt () = report t ~thread_id "next" in
      let%lwt _ = Dap_client.step_in t ~thread_id in
      let%lwt () = report t ~thread_id "step in" in
      let%lwt _ = Dap_client.step_out t ~thread_id in
      let%lwt () = report t ~thread_id "step out" in
      Lwt.return ())

let () = Lwt_main.run (main ())
