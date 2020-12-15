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
      if%lwt Lwt.return (not launch_args.stop_on_entry) then
        Ocaml_debug_agent.continue agent) ;
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
      let status_signal = Ocaml_debug_agent.status_signal agent in
      while%lwt true do
        let status = Lwt_react.S.value status_signal in
        let%lwt () =
          match status with
          | Exited ->
              Debug_rpc.send_event rpc
                (module Terminated_event)
                Terminated_event.Payload.(make ())
          | Entrypoint ->
              Debug_rpc.send_event rpc
                (module Stopped_event)
                Stopped_event.Payload.(make ~reason:Entry ~all_threads_stopped:(Some true) ())
          | Breakpoint ->
              Debug_rpc.send_event rpc
                (module Stopped_event)
                Stopped_event.Payload.(make ~reason:Breakpoint ~all_threads_stopped:(Some true) ())
          | Uncaught_exc ->
              Debug_rpc.send_event rpc
                (module Stopped_event)
                Stopped_event.Payload.(make ~reason:Exception ~all_threads_stopped:(Some true) ())
          | Running ->
              Lwt.return ()
        in
        Lwt_react.E.next
          ( status_signal |> Lwt_react.S.changes
          |> Lwt_react.E.fmap (function
               | Ocaml_debug_agent.Running ->
                   None
               | _ ->
                   Some ()) )
      done) ;
  Lwt.async (fun () -> Debug_rpc.send_event rpc (module Initialized_event) ()) ;
  promise
