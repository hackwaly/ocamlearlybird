open Debug_protocol_ex

let run ~terminate rpc =
  let (promise, resolve) = Lwt.task () in
  Debug_rpc.set_command_handler rpc (module Disconnect_command) (fun _ ->
    Debug_rpc.remove_command_handler rpc (module Disconnect_command);
    terminate ();%lwt
    Lwt.wakeup_later_exn resolve Exit;
    Lwt.return_unit
  );
  Debug_rpc.send_event rpc (module Initialized_event) ();%lwt
  promise
