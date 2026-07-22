(* Render one local per shape of OCaml value, and expand the structured ones a
   level to check their children. This covers the value_* renderers: ints,
   floats, chars, strings, bools, unit, boxed ints, lists, arrays, tuples,
   variants, records, closures and lazies. *)

open Debug_protocol

let main () =
  (* Line 34 is the [print_endline] at the end, by which point every local is
     bound. *)
  Dap_client.with_session ~program:"fixtures/values.bc"
    ~source:"fixtures/values.ml" ~breakpoints:[ 34 ] (fun t ->
      let%lwt stopped = Dap_client.wait_stopped t in
      let thread_id = Option.value stopped.thread_id ~default:0 in
      let%lwt frame = Dap_client.top_frame t ~thread_id in
      let%lwt variables = Dap_client.scope t ~frame ~name:"Stack" in
      variables
      |> Lwt_list.iter_s (fun (variable : Variable.t) ->
             Printf.printf "%-10s = %s\n" variable.name variable.value;
             let%lwt children = Dap_client.expand t ~variable in
             children
             |> List.iter (fun (child : Variable.t) ->
                    Printf.printf "  %s = %s\n" child.name child.value);
             Lwt.return ()))

let () = Lwt_main.run (main ())
