open Debug_protocol_ex

let run ~init_args ~launch_args ~dbg rpc =
  ignore init_args;
  let promise, resolver = Lwt.task () in
  Lwt.pause ();%lwt
  let send_initialize_event () =
    Debug_rpc.send_event rpc (module Initialized_event) ()
  in
  Debug_rpc.set_command_handler rpc
    (module Configuration_done_command)
    (fun _ ->
      let open Launch_command.Arguments in
      if not launch_args.stop_on_entry then Debugger.run dbg
      else
        Debug_rpc.send_event rpc
          (module Stopped_event)
          Stopped_event.Payload.(make ~reason:Entry ~thread_id:(Some 0) ()));
  Debug_rpc.set_command_handler rpc
    (module Terminate_command)
    (fun _ ->
      Debug_rpc.remove_command_handler rpc (module Terminate_command);
      Debugger.stop dbg);
  Debug_rpc.set_command_handler rpc
    (module Disconnect_command)
    (fun _ ->
      Debug_rpc.remove_command_handler rpc (module Disconnect_command);
      Debugger.stop_imm dbg;%lwt
      Lwt.wakeup_later_exn resolver Exit;
      Lwt.return_unit);
  Lwt.join [ send_initialize_event (); promise ]
