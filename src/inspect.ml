open Debugger
open Debug_protocol_ex

let run ~launch_args ~terminate ~agent rpc =
  ignore launch_args;
  ignore terminate;
  Lwt.pause ();%lwt
  let process_status_changes () =
    let process status =
      match status with
      | Unstarted ->
          Lwt.return ()
      | Exited _ ->
          Debug_rpc.send_event rpc
            (module Terminated_event)
            Terminated_event.Payload.(make ())
      | Stopped { breakpoint; _ } ->
          Debug_rpc.send_event rpc
            (module Stopped_event)
            Stopped_event.Payload.(
              make
                ~reason:(if breakpoint then Breakpoint else Step)
                ~thread_id:(Some 0) ())
      | Running -> Lwt.return ()
    in
    process (Debugger.status_signal agent |> Lwt_react.S.value);%lwt
    Debugger.status_signal agent
    |> Lwt_react.S.changes |> Lwt_react.E.to_stream |> Lwt_stream.iter_s process
  in
  Debug_rpc.set_command_handler rpc
    (module Loaded_sources_command)
    (fun () ->
      let modules = Debugger.to_seq_modules agent |> List.of_seq in
      let sources =
        modules
        |> List.filter (fun module_ ->
               module_.Debugger.Module.source |> Option.is_some)
        |> List.map (fun module_ ->
               Source.make ~path:module_.Debugger.Module.source ())
      in
      Loaded_sources_command.Result.make ~sources () |> Lwt.return);
  Debug_rpc.set_command_handler rpc
    (module Threads_command)
    (fun () ->
      let main_thread = Thread.make ~id:0 ~name:"main" in
      Lwt.return (Threads_command.Result.make ~threads:[ main_thread ] ()));
  Debug_rpc.set_command_handler rpc
    (module Stack_trace_command)
    (fun arg ->
      assert (arg.thread_id = 0);
      let%lwt frames =
        match agent |> Debugger.status_signal |> Lwt_react.S.value with
        | Stopped _ ->
            let frames = ref [] in
            let%lwt frame0 = Debugger.initial_frame agent in
            let rec walk frame =
              frames := frame :: !frames;
              match arg.levels with
              | Some levels when levels <> 0 && frame.Frame.index + 1 >= levels
                ->
                  Lwt.return ()
              | _ -> (
                  let%lwt frame' = Debugger.up_frame agent frame in
                  match frame' with
                  | Some frame' -> walk frame'
                  | None -> Lwt.return () )
            in
            (walk frame0) [%finally Debugger.set_frame agent frame0];%lwt
            let frames = List.rev !frames in
            Lwt.return frames
        | _ -> Lwt.return []
      in
      let%lwt stack_frames =
        frames
        |> Lwt_list.map_s (fun fr ->
               let module_ = Frame.module_ fr in
               let source =
                 Source.(make ~path:module_.Debugger.Module.source ())
               in
               let frame =
                 let loc = Frame.loc fr in
                 Stack_frame.(
                   make ~id:fr.index ~name:(Frame.defname fr)
                     ~source:(Some source) ~line:loc.loc_start.pos_lnum
                     ~column:(loc.loc_start.pos_cnum - loc.loc_start.pos_bol + 1)
                     ~end_line:(Some loc.loc_end.pos_lnum)
                     ~end_column:
                       (Some (loc.loc_end.pos_cnum - loc.loc_end.pos_bol + 1))
                     ())
               in
               Lwt.return frame)
      in
      Lwt.return
        Stack_trace_command.Result.(
          make ~stack_frames ~total_frames:(Some (List.length frames)) ()));
  Debug_rpc.set_command_handler rpc
    (module Scopes_command)
    (fun arg ->
      let scopes = [] in
      Lwt.return Scopes_command.Result.(make ~scopes ()));
  Debug_rpc.set_command_handler rpc
    (module Variables_command)
    (fun arg ->
      let variables = [] in
      Lwt.return Variables_command.Result.(make ~variables ()));
  Lwt.join [ process_status_changes () ]
