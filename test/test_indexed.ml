(* Expand collections with a single, unfiltered [variables] request — the way a
   minimal DAP client that does not read the named/indexed child counts does.

   The DAP spec says an omitted filter returns both named and indexed children.
   This checks the unfiltered path returns the children for a value with named
   children (a list), indexed children (an array), and both (the adapter exposes
   an array's length as a named child). *)

open Debug_protocol

let main () =
  Dap_client.with_session ~program:"fixtures/values.bc"
    ~source:"fixtures/values.ml" ~breakpoints:[ 34 ] (fun t ->
      let%lwt stopped = Dap_client.wait_stopped t in
      let thread_id = Option.value stopped.thread_id ~default:0 in
      let%lwt frame = Dap_client.top_frame t ~thread_id in
      let%lwt locals = Dap_client.scope t ~frame ~name:"Stack" in
      [ "list_"; "array_"; "record_" ]
      |> Lwt_list.iter_s (fun name ->
             match
               List.find_opt (fun (v : Variable.t) -> v.name = name) locals
             with
             | None -> Lwt.fail_with (Printf.sprintf "no local %s" name)
             | Some variable ->
                 let%lwt children = Dap_client.children_unfiltered t ~variable in
                 let children =
                   children
                   |> List.map (fun (c : Variable.t) ->
                          Printf.sprintf "%s = %s" c.name c.value)
                 in
                 Printf.printf "%s (%s) -> %s\n" name variable.value
                   (String.concat ", " children);
                 Lwt.return ()))

let () = Lwt_main.run (main ())
