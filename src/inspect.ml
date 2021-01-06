open Debugger
open Debug_protocol_ex

type handle = Scope of Frame.t * [ `Stack | `Heap ] | Value of Value.t

let run ~launch_args ~terminate ~agent rpc =
  ignore launch_args;
  ignore terminate;
  let frame_tbl = Hashtbl.create 0 in
  let alloc_handle = Unique_id.make_alloc 1 in
  let handle_tbl = Hashtbl.create 0 in
  Lwt.pause ();%lwt
  let process_status_changes () =
    let process status =
      Hashtbl.reset frame_tbl;
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
              Hashtbl.replace frame_tbl frame.Frame.index frame;
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
      let frame = Hashtbl.find frame_tbl arg.frame_id in
      let alloc_scope kind =
        let handle = alloc_handle () in
        Hashtbl.replace handle_tbl handle (Scope (frame, kind));
        let name = match kind with `Stack -> "Stack" | `Heap -> "Heap" in
        Scope.make ~name ~expensive:false ~variables_reference:handle ()
      in
      let scopes = [ alloc_scope `Stack; alloc_scope `Heap ] in
      Lwt.return Scopes_command.Result.(make ~scopes ()));
  Debug_rpc.set_command_handler rpc
    (module Variables_command)
    (fun arg ->
      let handle_desc = Hashtbl.find handle_tbl arg.variables_reference in
      let alloc_variable (ident, value) =
        let handle =
          if Value.is_named_container value || Value.is_indexed_container value
          then (
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
        Variable.make ~name:(Ident.name ident)
          ~value:(Value.to_short_string ~hex value)
          ~indexed_variables:
            ( if Value.is_indexed_container value then
              Some (Value.num_indexed value)
            else None )
          ~variables_reference:handle ()
      in
      let%lwt variables =
        match handle_desc with
        | Scope (frame, kind) -> Debugger.frame_variables agent frame kind
        | Value value -> (
            let named = Value.is_named_container value in
            let indexed = Value.is_indexed_container value in
            match (arg.filter, named, indexed) with
            | _, true, true -> Lwt.fail_with "Not supported"
            | (Some Indexed | None), false, true ->
                let num_indexed = Value.num_indexed value in
                let start, count =
                  match (arg.start, arg.count) with
                  | _, None -> (0, num_indexed)
                  | start, Some count ->
                      (start |> Option.value ~default:0, count)
                in
                let start = min num_indexed start in
                let count = min num_indexed count in
                let values = ref [] in
                for%lwt i = start to start + count - 1 do
                  let%lwt value' = Value.get_indexed value i in
                  values := value' :: !values;
                  Lwt.return ()
                done;%lwt
                let values = List.rev !values in
                let variables =
                  values
                  |> List.mapi (fun i v ->
                         (Ident.create_local (string_of_int (start + i)), v))
                in
                Lwt.return variables
            | (Some Named | None), true, false -> Lwt.return []
            | _ -> Lwt.return [] )
      in
      let variables = variables |> List.map alloc_variable in
      Lwt.return Variables_command.Result.(make ~variables ()));
  Lwt.join [ process_status_changes () ]
