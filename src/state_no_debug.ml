open Debug_protocol_ex

let run ~launch_args ~terminate rpc =
  ignore launch_args;
  let promise, resolver = Lwt.task () in
  Debug_rpc.set_command_handler rpc
    (module Terminate_command)
    (fun _ ->
      Debug_rpc.remove_command_handler rpc (module Terminate_command);
      Lwt.async (fun () ->
          terminate false;%lwt
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
  Debug_rpc.send_event rpc (module Initialized_event) ();%lwt
  promise
