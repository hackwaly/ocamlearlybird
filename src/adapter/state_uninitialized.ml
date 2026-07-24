open Debug_protocol_ex

let run rpc =
  let promise, resolver = Lwt.task () in
  let prevent_reenter () =
    Debug_rpc.remove_command_handler rpc (module Initialize_command)
  in
  Debug_rpc.set_command_handler rpc
    (module Initialize_command)
    (fun arg ->
      prevent_reenter ();
      let caps =
        Capabilities.(
          make ~supports_terminate_request:(Some true)
            ~supports_configuration_done_request:(Some true)
            ~supports_loaded_sources_request:(Some true)
            ~supports_delayed_stack_trace_loading:(Some true)
            ~supports_breakpoint_locations_request:(Some true)
            ~supports_value_formatting_options:(Some true) ())
      in
      (* Wake immediately so that the next state registers
         its Launch/Attach handlers synchronously, before this handler returns
         and the initialize response is sent. Otherwise a client that pipelines
         "launch" right after "initialize" can have it arrive before the handler
         exists, and it is silently dropped. *)
      Lwt.wakeup resolver (arg, caps);
      Lwt.return caps);
  promise
