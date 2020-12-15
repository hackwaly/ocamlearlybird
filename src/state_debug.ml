open Debug_protocol_ex

let src = Logs.Src.create "earlybird.State_debug"

module Log = (val Logs_lwt.src_log src : Logs_lwt.LOG)

let run ~launch_args ~terminate ~agent rpc =
  let promise, resolver = Lwt.task () in
  Debug_rpc.set_command_handler rpc
    (module Loaded_sources_command)
    (fun () ->
      let%lwt sources = Ocaml_debug_agent.sources agent in
      let sources =
        sources |> List.map (fun source -> Source.make ~path:(Some source) ())
      in
      Loaded_sources_command.Result.make ~sources () |> Lwt.return) ;
  Debug_rpc.set_command_handler rpc
    (module Threads_command)
    (fun () ->
      let main_thread = Thread.make ~id:0 ~name:"main" in
      Lwt.return (Threads_command.Result.make ~threads:[main_thread] ())) ;
  Debug_rpc.set_command_handler rpc
    (module Set_breakpoints_command)
    (fun args ->
      let breakpoints =
        args.breakpoints |> Option.to_list
        |> List.map (fun _ -> Breakpoint.make ~verified:false ())
      in
      Lwt.return Set_breakpoints_command.Result.(make ~breakpoints ())) ;
  Debug_rpc.set_command_handler rpc
    (module Set_exception_breakpoints_command)
    (fun _ -> Lwt.return_unit) ;
  Debug_rpc.set_command_handler rpc
    (module Configuration_done_command)
    (fun _ ->
      let open Launch_command.Arguments in
      if launch_args.stop_on_entry then
        Debug_rpc.send_event rpc
          (module Stopped_event)
          Stopped_event.Payload.(
            make ~reason:Reason.Entry ~all_threads_stopped:(Some true) ())
      else Ocaml_debug_agent.continue agent ;%lwt
      Lwt.return_unit) ;
  Debug_rpc.set_command_handler rpc
    (module Continue_command)
    (fun _ ->
      Ocaml_debug_agent.continue agent ;%lwt
      Lwt.return
        Continue_command.Result.(make ~all_threads_continued:(Some true) ())) ;
  Debug_rpc.set_command_handler rpc
    (module Pause_command)
    (fun _ ->
      Ocaml_debug_agent.pause agent ;%lwt
      Lwt.return ()) ;
  Debug_rpc.set_command_handler rpc
    (module Terminate_command)
    (fun _ ->
      Debug_rpc.remove_command_handler rpc (module Terminate_command) ;
      Lwt.async (fun () ->
          terminate false ;%lwt
          Debug_rpc.send_event rpc
            (module Terminated_event)
            Terminated_event.Payload.(make ())) ;
      Lwt.return_unit) ;
  Debug_rpc.set_command_handler rpc
    (module Disconnect_command)
    (fun _ ->
      Debug_rpc.remove_command_handler rpc (module Disconnect_command) ;
      terminate true ;%lwt
      Lwt.wakeup_later_exn resolver Exit ;
      Lwt.return_unit) ;
  Lwt.async (fun () ->
      let status_event =
        ref (Ocaml_debug_agent.status_signal agent |> Lwt_react.S.changes)
      in
      while%lwt true do
      Logs_lwt.debug (fun m -> m "status_event 1") ;%lwt
        let%lwt status = Lwt_react.E.next !status_event in
        Logs_lwt.debug (fun m -> m "status_event") ;%lwt
        let%lwt () =
          match status with
          | Exited ->
            Logs_lwt.debug (fun m -> m "status_event Exited") ;%lwt
              Debug_rpc.send_event rpc
                (module Terminated_event)
                Terminated_event.Payload.(make ())
          | Entrypoint ->
            Logs_lwt.debug (fun m -> m "status_event Entrypoint") ;%lwt
              Debug_rpc.send_event rpc
                (module Stopped_event)
                Stopped_event.Payload.(make ~reason:Entry ())
          | Breakpoint ->
            Logs_lwt.debug (fun m -> m "status_event Breakpoint") ;%lwt
              Debug_rpc.send_event rpc
                (module Stopped_event)
                Stopped_event.Payload.(make ~reason:Breakpoint ())
          | Uncaught_exc ->
            Logs_lwt.debug (fun m -> m "status_event Uncaught_exc") ;%lwt
              Debug_rpc.send_event rpc
                (module Stopped_event)
                Stopped_event.Payload.(make ~reason:Exception ())
          | Running ->
            Logs_lwt.debug (fun m -> m "status_event Running") ;%lwt
              Lwt.return ()
        in
        status_event := Lwt_react.E.drop_once !status_event ;
        Lwt.return_unit
      done) ;
  Lwt.async (fun () -> Ocaml_debug_agent.continue agent) ;
  promise
