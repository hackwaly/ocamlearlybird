open Debug_protocol_ex

let src = Logs.Src.create "earlybird.State_debug"

module Log = (val Logs_lwt.src_log src : Logs_lwt.LOG)

module StringToMultiIntMap = CCMultiMap.Make (CCString) (CCInt)

type breakpoint_desc = {
  id : int;
  src_pos : Ocaml_debug_agent.src_pos;
  is_line_brekpoint : bool;
  loop_promise : unit Lwt.t option ref;
  resolved :
    (Ocaml_debug_agent.src_pos * Ocaml_debug_agent.Breakpoint.t) option ref;
}

let run ~launch_args ~terminate ~agent rpc =
  let promise, resolver = Lwt.task () in
  let alloc_breakpoint_id = Unique_id.make_alloc () in
  let breakpoint_tbl = Hashtbl.create 0 in
  let module_breakpoints_map = ref StringToMultiIntMap.empty in
  Debug_rpc.set_command_handler rpc
    (module Loaded_sources_command)
    (fun () ->
      let%lwt sources = Ocaml_debug_agent.sources agent in
      let sources =
        sources |> List.map (fun source -> Source.make ~path:(Some source) ())
      in
      Loaded_sources_command.Result.make ~sources () |> Lwt.return);
  Debug_rpc.set_command_handler rpc
    (module Threads_command)
    (fun () ->
      let main_thread = Thread.make ~id:0 ~name:"main" in
      Lwt.return (Threads_command.Result.make ~threads:[ main_thread ] ()));
  Debug_rpc.set_command_handler rpc
    (module Set_breakpoints_command)
    (fun args ->
      let source_path = args.source.path |> Option.get in
      let make_desc src_bp =
        let id = alloc_breakpoint_id () in
        let src_pos =
          Ocaml_debug_agent.
            {
              source = source_path;
              line = src_bp.Source_breakpoint.line;
              column =
                src_bp.Source_breakpoint.column |> Option.value ~default:0;
            }
        in
        let is_line_brekpoint =
          src_bp.Source_breakpoint.column |> Option.is_none
        in
        let resolved = ref None in
        { id; src_pos; is_line_brekpoint; loop_promise = ref None; resolved }
      in
      let set_breakpoint desc =
        let src_pos = desc.src_pos in
        let id = desc.id in
        let resolved = desc.resolved in
        let loop () =
          Lwt.pause ();%lwt
          while%lwt Option.is_none !resolved do
            match%lwt Ocaml_debug_agent.resolve agent src_pos with
            | Some (pc, src_pos') ->
                Log.debug (fun m ->
                    m "resolve success src_pos: %s"
                      (Ocaml_debug_agent.show_src_pos src_pos));%lwt
                let breakpoint = Ocaml_debug_agent.Breakpoint.make ~id ~pc () in
                resolved := Some (src_pos', breakpoint);
                Lwt.return ()
            | None ->
                Log.debug (fun m ->
                    m "resolve failed src_pos: %s"
                      (Ocaml_debug_agent.show_src_pos src_pos));%lwt
                Lwt_react.E.next (Ocaml_debug_agent.symbols_change_event agent);%lwt
                Lwt.pause ()
          done;%lwt
          let resolved_src_pos, breakpoint = !resolved |> Option.get in
          Ocaml_debug_agent.set_breakpoint agent breakpoint;%lwt
          while%lwt true do
            let active_signal =
              Ocaml_debug_agent.Breakpoint.active_signal breakpoint
            in
            let active = Lwt_react.S.value active_signal in
            if active then
              Debug_rpc.send_event rpc
                (module Breakpoint_event)
                Breakpoint_event.Payload.(
                  make ~reason:Changed
                    ~breakpoint:
                      Breakpoint.(
                        make ~id:(Some id) ~verified:true
                          ~source:
                            (Some
                               Source.(
                                 make ~path:(Some resolved_src_pos.source) ()))
                          ~line:
                            (Some
                               ( if desc.is_line_brekpoint then src_pos.line
                               else resolved_src_pos.line ))
                          ~column:
                            ( if desc.is_line_brekpoint then None
                            else if resolved_src_pos.column = 0 then None
                            else Some resolved_src_pos.column )
                          ()))
            else
              Debug_rpc.send_event rpc
                (module Breakpoint_event)
                Breakpoint_event.Payload.(
                  make ~reason:Changed
                    ~breakpoint:
                      (Breakpoint.make ~id:(Some id) ~verified:false ()));%lwt
            let%lwt _ =
              Lwt_react.E.next (active_signal |> Lwt_react.S.changes)
            in
            Lwt.return ()
          done
        in
        let loop_promise = loop () in
        desc.loop_promise := Some loop_promise;
        Lwt.async (fun () ->
            try%lwt loop_promise with Lwt.Canceled -> Lwt.return ());
        Hashtbl.replace breakpoint_tbl id desc;
        module_breakpoints_map :=
          StringToMultiIntMap.add !module_breakpoints_map source_path id;
        Breakpoint.make ~id:(Some id) ~verified:false ()
      in
      let prev =
        StringToMultiIntMap.find !module_breakpoints_map source_path
        |> List.map (fun id -> Hashtbl.find breakpoint_tbl id)
      in
      Lwt_list.iter_s
        (fun desc ->
          Hashtbl.remove breakpoint_tbl desc.id;
          if%lwt Lwt.return (Option.is_some desc.resolved.contents) then (
            let _, bp = desc.resolved.contents |> Option.get in
            Ocaml_debug_agent.remove_breakpoint agent bp;%lwt
            desc.resolved := None;
            Lwt.return () );%lwt
          if Option.is_some desc.loop_promise.contents then
            Lwt.cancel (desc.loop_promise.contents |> Option.get);
          Lwt.return ())
        prev;%lwt
      module_breakpoints_map :=
        StringToMultiIntMap.remove_all !module_breakpoints_map source_path;
      let next =
        args.breakpoints |> Option.value ~default:[] |> List.map make_desc
      in
      let breakpoints = next |> List.map set_breakpoint in
      Lwt.return Set_breakpoints_command.Result.(make ~breakpoints ()));
  Debug_rpc.set_command_handler rpc
    (module Set_exception_breakpoints_command)
    (fun _ -> Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Configuration_done_command)
    (fun _ ->
      let open Launch_command.Arguments in
      if%lwt Lwt.return (not launch_args.stop_on_entry) then
        Ocaml_debug_agent.continue agent);
  Debug_rpc.set_command_handler rpc
    (module Continue_command)
    (fun _ ->
      Ocaml_debug_agent.continue agent;%lwt
      Lwt.return
        Continue_command.Result.(make ~all_threads_continued:(Some true) ()));
  Debug_rpc.set_command_handler rpc
    (module Pause_command)
    (fun _ ->
      Ocaml_debug_agent.pause agent;%lwt
      Lwt.return ());
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
  Lwt.async (fun () ->
      let%lwt signal =
        Ocaml_debug_agent.status_signal agent
        |> Lwt_react.S.map_s (fun status ->
               match status with
               | Ocaml_debug_agent.Exited ->
                   Debug_rpc.send_event rpc
                     (module Terminated_event)
                     Terminated_event.Payload.(make ())
               | Entrypoint ->
                   Debug_rpc.send_event rpc
                     (module Stopped_event)
                     Stopped_event.Payload.(
                       make ~reason:Entry ~all_threads_stopped:(Some true) ())
               | Breakpoint ->
                   Debug_rpc.send_event rpc
                     (module Stopped_event)
                     Stopped_event.Payload.(
                       make ~reason:Breakpoint ~all_threads_stopped:(Some true)
                         ())
               | Uncaught_exc ->
                   Debug_rpc.send_event rpc
                     (module Stopped_event)
                     Stopped_event.Payload.(
                       make ~reason:Exception ~all_threads_stopped:(Some true)
                         ())
               | Running -> Lwt.return ())
      in
      Lwt_react.S.keep signal;
      Lwt.return ());
  Lwt.async (fun () -> Debug_rpc.send_event rpc (module Initialized_event) ());
  promise
