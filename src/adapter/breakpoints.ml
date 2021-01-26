open Debug_protocol_ex

let run ~launch_args ~dbg rpc =
  ignore launch_args;
  ignore dbg;
  Lwt.pause ();%lwt
  Debug_rpc.set_command_handler rpc
    (module Set_breakpoints_command)
    (fun args ->
      ignore args;
      Lwt.return Set_breakpoints_command.Result.(make ~breakpoints:[] ()));
  Debug_rpc.set_command_handler rpc
    (module Breakpoint_locations_command)
    (fun arg ->
      ignore arg;
      Lwt.return Breakpoint_locations_command.Result.(make ~breakpoints:[] ())
    );
  Lwt.join []
