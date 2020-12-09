open Debug_protocol_ex

let src = Logs.Src.create "earlybird.State_debug"
module Log = (val Logs_lwt.src_log src : Logs_lwt.LOG)

let run ~terminate ~agent rpc =
  let (promise, resolver) = Lwt.task () in
  Debug_rpc.set_command_handler rpc (module Loaded_sources_command) (fun _ ->
    let sources = Debug_agent.loaded_sources agent in
    Lwt.return Loaded_sources_command.Result.(make ~sources ())
  );
  Debug_rpc.set_command_handler rpc (module Threads_command) (fun _ ->
    let main_thread = Thread.make ~id:0 ~name:"main" in
    Lwt.return (Threads_command.Result.make ~threads:[main_thread] ())
  );
  Debug_rpc.set_command_handler rpc (module Set_breakpoints_command) (fun arg ->
    let breakpoints = arg.breakpoints |> Option.to_list |> List.map (fun _ ->
      Breakpoint.make ~verified:false ()
    ) in
    Lwt.return Set_breakpoints_command.Result.(make ~breakpoints:breakpoints ())
  );
  Debug_rpc.set_command_handler rpc (module Set_exception_breakpoints_command) (fun _ ->
    Lwt.return_unit
  );
  Debug_rpc.set_command_handler rpc (module Terminate_command) (fun _ ->
    Debug_rpc.remove_command_handler rpc (module Terminate_command);
    Lwt.async (fun () ->
      terminate false;%lwt
      Debug_rpc.send_event rpc (module Terminated_event) Terminated_event.Payload.(make ())
    );
    Lwt.return_unit
  );
  Debug_rpc.set_command_handler rpc (module Disconnect_command) (fun _ ->
    Debug_rpc.remove_command_handler rpc (module Disconnect_command);
    terminate true;%lwt
    Lwt.wakeup_later_exn resolver Exit;
    Lwt.return_unit
  );
  Lwt.async (fun () ->
    let stopped_event = ref (Debug_agent.stopped_event agent) in
    while%lwt true do
      let%lwt report = Lwt_react.E.next !stopped_event in
      let%lwt () = match report.rep_type with
        | Exited -> (
          Debug_rpc.send_event rpc (module Terminated_event) Terminated_event.Payload.(make ());
        )
        | _ -> Lwt.return_unit
      in
      stopped_event := Lwt_react.E.drop_once !stopped_event;
      Lwt.return_unit
    done
  );
  Lwt.async (fun () ->
    Debug_agent.load agent;%lwt
    Debug_rpc.send_event rpc (module Initialized_event) ();%lwt
    Debug_agent.start agent
  );
  promise
