open Debug_protocol_ex

let run ~launch_args ~dbg rpc =
  ignore launch_args;
  Lwt.pause ();%lwt
  Debug_rpc.set_command_handler rpc
    (module Pause_command)
    (fun _ ->
      Debugger.pause dbg);
  Debug_rpc.set_command_handler rpc
    (module Continue_command)
    (fun _ ->
      Debugger.run dbg;%lwt
      Lwt.return (Continue_command.Result.make ()));
  Debug_rpc.set_command_handler rpc
    (module Step_in_command)
    (fun _ ->
      Debugger.step_in dbg);
  Debug_rpc.set_command_handler rpc
    (module Step_out_command)
    (fun _ ->
      Debugger.step_out dbg);
  Debug_rpc.set_command_handler rpc
    (module Next_command)
    (fun _ ->
      Debugger.next dbg);
  Lwt.return ()

