open Debug_protocol_ex

let src = Logs.Src.create "earlybird.State_debug"
module Log = (val Logs_lwt.src_log src : Logs_lwt.LOG)

let run ~launch_args ~terminate ~agent rpc =
  let (promise, resolver) = Lwt.task () in



  Debug_rpc.set_command_handler rpc (module Loaded_sources_command) (fun () ->
    let%lwt sources = Debug_agent.loaded_sources agent in
    Loaded_sources_command.Result.make ~sources () |> Lwt.return
  );

  Debug_rpc.set_command_handler rpc (module Threads_command) (fun () ->
    let main_thread = Thread.make ~id:0 ~name:"main" in
    Lwt.return (Threads_command.Result.make ~threads:[main_thread] ())
  );

  Debug_rpc.set_command_handler rpc (module Set_breakpoints_command) (fun args ->
    let breakpoints = args.breakpoints |> Option.to_list |> List.map (fun _ ->
      Breakpoint.make ~verified:false ()
    ) in
    Lwt.return Set_breakpoints_command.Result.(make ~breakpoints:breakpoints ())
  );

  Debug_rpc.set_command_handler rpc (module Set_exception_breakpoints_command) (fun _ ->
    Lwt.return_unit
  );

  Debug_rpc.set_command_handler rpc (module Configuration_done_command) (fun _ ->
    let open Launch_command.Arguments in
    if launch_args.stop_on_entry then (
      Debug_rpc.send_event rpc (module Stopped_event) Stopped_event.Payload.(
        make ~reason:Reason.Entry ~all_threads_stopped:(Some true) ()
      );
    ) else (
      Debug_agent.continue_main_thread agent;
      Lwt.return_unit
    );%lwt
    Lwt.return_unit
  );

  Debug_rpc.set_command_handler rpc (module Continue_command) (fun _ ->
    Debug_agent.continue_main_thread agent;
    Lwt.return Continue_command.Result.(make ~all_threads_continued:(Some true) ())
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
