open Debug_protocol_ex

let run ~launch_args ~terminate ~agent rpc =
  ignore launch_args;
  ignore terminate;
  Lwt.pause ();%lwt
  Debug_rpc.set_command_handler rpc
    (module Continue_command)
    (fun _ ->
      Debugger.run agent;
      Lwt.return
        Continue_command.Result.(make ()));
  Debug_rpc.set_command_handler rpc
    (module Pause_command)
    (fun _ ->
      Debugger.pause agent;
      Lwt.return ());
  Debug_rpc.set_command_handler rpc
    (module Step_in_command)
    (fun _ ->
      Debugger.step_in agent;
      Lwt.return ());
  Debug_rpc.set_command_handler rpc
    (module Step_out_command)
    (fun _ ->
      Debugger.step_out agent;
      Lwt.return ());
  Debug_rpc.set_command_handler rpc
    (module Next_command)
    (fun _ ->
      Debugger.step_over agent;
      Lwt.return ());
  Lwt.return ()
