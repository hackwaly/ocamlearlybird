open Debug_protocol_ex

let run rpc =
  let (promise, resolver) = Lwt.task () in
  let prevent_reenter () =
    Debug_rpc.remove_command_handler rpc (module Initialize_command);
  in
  Debug_rpc.set_command_handler rpc (module Initialize_command) (fun arg ->
    prevent_reenter ();
    let caps = Capabilities.(
      make
        ~supports_terminate_request:(Some true)
        ~supports_configuration_done_request:(Some true)
        ~supports_loaded_sources_request:(Some true)
        ()
    ) in
    Lwt.wakeup_later resolver (arg, caps);
    Lwt.return caps
  );
  promise
