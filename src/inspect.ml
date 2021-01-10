open Debugger
open Debug_protocol_ex

type handle =
  | Scope of Frame.t * [ `Stack | `Heap | `Global ]
  | Value of Value.t

let run ~launch_args ~terminate ~agent rpc =
  ignore launch_args;
  ignore terminate;
  let frame_tbl = Hashtbl.create 0 in
  let alloc_frame_id = Unique_id.make_alloc 1 in
  let alloc_handle = Unique_id.make_alloc 1 in
  let handle_tbl = Hashtbl.create 0 in
  Lwt.pause ();%lwt
  let process_status_changes () =
    let process status =
      Hashtbl.reset frame_tbl;
      Hashtbl.reset handle_tbl;
      match status with
      | Unstarted -> Lwt.return ()
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
    Debugger.ready agent;%lwt
    Debugger.status_signal agent
    |> Lwt_react.S.changes |> Lwt_react.E.to_stream |> Lwt_stream.iter_s process
  in
  Debug_rpc.set_command_handler rpc
    (module Loaded_sources_command)
    (fun () ->
      Debugger.ready agent;%lwt
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
            walk frame0;%lwt
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
               let id = alloc_frame_id () in
               Hashtbl.replace frame_tbl id fr;
               let frame =
                 let loc = Frame.loc fr in
                 Stack_frame.(
                   make ~id ~name:(Frame.defname fr) ~source:(Some source)
                     ~line:loc.loc_start.pos_lnum
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
      let scopes =
        match Hashtbl.find frame_tbl arg.frame_id with
        | frame ->
            let alloc_scope kind =
              let handle = alloc_handle () in
              Hashtbl.replace handle_tbl handle (Scope (frame, kind));
              let name =
                match kind with
                | `Stack -> "Stack"
                | `Heap -> "Heap"
                | `Global -> "Global"
              in
              Scope.make ~name ~expensive:false ~variables_reference:handle ()
            in
            [ alloc_scope `Stack; alloc_scope `Heap; alloc_scope `Global ]
        | exception Not_found -> []
      in
      Lwt.return Scopes_command.Result.(make ~scopes ()));
  Debug_rpc.set_command_handler rpc
    (module Variables_command)
    (fun arg ->
      let%lwt variables =
        match Hashtbl.find handle_tbl arg.variables_reference with
        | handle_desc ->
            let alloc_variable (name, value) =
              let num_named = Value.num_named value in
              let handle =
                if num_named <> 0 || Value.is_indexed_container value then (
                  let handle = alloc_handle () in
                  Hashtbl.replace handle_tbl handle (Value value);
                  handle )
                else 0
              in
              let hex =
                match arg.format with
                | None -> false
                | Some format -> format.hex |> Option.value ~default:false
              in
              Variable.make ~name
                ~value:(Value.to_short_string ~hex value)
                ~named_variables:(if num_named > 0 then Some num_named else None)
                ~indexed_variables:
                  ( if Value.is_indexed_container value then
                    Some (Value.num_indexed value)
                  else None )
                ~variables_reference:handle ()
            in
            let%lwt variables =
              match handle_desc with
              | Scope (frame, kind) -> Debugger.list_variables agent frame kind
              | Value value -> (
                  let list_indexed start count =
                    let values = ref [] in
                    for%lwt i = start to start + count - 1 do
                      let%lwt value' = Value.get_indexed value i in
                      values := value' :: !values;
                      Lwt.return ()
                    done;%lwt
                    let values = List.rev !values in
                    let variables =
                      values
                      |> List.mapi (fun i v -> (string_of_int (start + i), v))
                    in
                    Lwt.return variables
                  in
                  let num_named = Value.num_named value in
                  if num_named = -1 then Value.list_named value
                  else
                    let num_indexed = Value.num_indexed value in
                    match arg.filter with
                    | None ->
                        [%lwt assert (arg.count |> Option.is_none)];%lwt
                        if num_named > 0 && num_indexed > 0 then
                          let%lwt named = Value.list_named value in
                          let%lwt indexed = list_indexed 0 num_indexed in
                          Lwt.return (named @ indexed)
                        else if num_named > 0 then
                          let%lwt named = Value.list_named value in
                          Lwt.return named
                        else if num_indexed > 0 then
                          let%lwt indexed = list_indexed 0 num_indexed in
                          Lwt.return indexed
                        else Lwt.return []
                    | Some Indexed ->
                        let start, count =
                          match (arg.start, arg.count) with
                          | _, None -> (0, num_indexed)
                          | start, Some count ->
                              (start |> Option.value ~default:0, count)
                        in
                        let%lwt indexed = list_indexed start count in
                        Lwt.return indexed
                    | Some Named ->
                        let%lwt named = Value.list_named value in
                        Lwt.return named )
            in
            let variables = variables |> List.map alloc_variable in
            Lwt.return variables
        | exception Not_found -> Lwt.return []
      in
      Lwt.return Variables_command.Result.(make ~variables ()));
  Lwt.join [ process_status_changes () ]
