open Debug_protocol_ex

let run ~launch_args ~terminate ~agent rpc =
  let promise, resolver = Lwt.task () in
  Lwt.pause ();%lwt
  let send_initialize_event () =
    Debug_rpc.send_event rpc (module Initialized_event) ()
  in
  Debug_rpc.set_command_handler rpc
    (module Configuration_done_command)
    (fun _ ->
      let open Launch_command.Arguments in
      if (not launch_args.stop_on_entry) then
        Ocaml_debug_agent.run agent;
      Lwt.return ());
  Debug_rpc.set_command_handler rpc
    (module Terminate_command)
    (fun _ ->
      Debug_rpc.remove_command_handler rpc (module Terminate_command);
      Lwt.async (fun () ->
          Ocaml_debug_agent.stop agent;
          Debug_rpc.send_event rpc
            (module Terminated_event)
            Terminated_event.Payload.(make ()));
      Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Disconnect_command)
    (fun _ ->
      Debug_rpc.remove_command_handler rpc (module Disconnect_command);
      terminate true;%lwt
      Lwt.wakeup_later_exn resolver Exit;
      Lwt.return_unit);
  Lwt.join [
    send_initialize_event ();
    Ocaml_debug_agent.start agent;
    promise
  ]
