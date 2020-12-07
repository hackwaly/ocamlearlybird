open Debug_protocol_ex

let run ~terminate ~(symbols : Debug_symbols.t) ~com rpc =
  let (promise, resolve) = Lwt.task () in
  Debug_rpc.set_command_handler rpc (module Set_breakpoints_command) (fun arg ->
    let breakpoints = arg.breakpoints |> Option.to_list |> List.map (fun _ ->
      Breakpoint.make ~verified:false ()
    ) in
    Lwt.return Set_breakpoints_command.Result.(make ~breakpoints:breakpoints ())
  );
  Debug_rpc.set_command_handler rpc (module Set_exception_breakpoints_command) (fun _ ->
    Lwt.return_unit
  );
  Debug_rpc.set_command_handler rpc (module Threads_command) (fun _ ->
    let main_thread = Thread.make ~id:0 ~name:"main" in
    Lwt.return (Threads_command.Result.make ~threads:[main_thread] ())
  );
  Debug_rpc.set_command_handler rpc (module Disconnect_command) (fun _ ->
    Debug_rpc.remove_command_handler rpc (module Disconnect_command);
    terminate ();%lwt
    Lwt.wakeup_later_exn resolve Exit;
    Lwt.return_unit
  );
  Lwt.async (fun () ->
    Hashtbl.to_seq_keys symbols.event_by_pc
    |> Seq.fold_left (fun wait pc ->
      let%lwt () = wait in
      Debug_com.set_event com pc
    ) Lwt.return_unit;%lwt
    Debug_rpc.send_event rpc (module Initialized_event) ();
  );
  promise
