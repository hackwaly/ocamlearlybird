open Debug_protocol_ex
open Ocaml_debug_agent
module Dap_breakpoint = Debug_protocol_ex.Breakpoint
module Dap_stack_frame = Debug_protocol_ex.Stack_frame

let run ~launch_args ~terminate ~agent rpc =
  ignore launch_args;
  ignore terminate;
  Lwt.pause ();%lwt
  let process_status_changes () =
    let process status =
      match status with
      | Entry ->
          if launch_args.Launch_command.Arguments.stop_on_entry then
            Debug_rpc.send_event rpc
              (module Stopped_event)
              Stopped_event.Payload.(
                make ~reason:Entry ~thread_id:(Some 0) ())
          else Lwt.return ()
      | Exited _ ->
          Debug_rpc.send_event rpc
            (module Terminated_event)
            Terminated_event.Payload.(make ())
      | Stopped { breakpoint } ->
          Debug_rpc.send_event rpc
            (module Stopped_event)
            Stopped_event.Payload.(
              make
                ~reason:(if breakpoint then Breakpoint else Step)
                ~thread_id:(Some 0) ())
      | Running -> Lwt.return ()
    in
    process (Ocaml_debug_agent.status_signal agent |> Lwt_react.S.value);%lwt
    Ocaml_debug_agent.status_signal agent
    |> Lwt_react.S.changes |> Lwt_react.E.to_stream |> Lwt_stream.iter_s process
  in
  Debug_rpc.set_command_handler rpc
    (module Loaded_sources_command)
    (fun () ->
      let modules = Ocaml_debug_agent.to_seq_modules agent |> List.of_seq in
      let sources =
        modules
        |> List.filter (fun mi -> mi.Module.resolved_source |> Option.is_some)
        |> List.map (fun mi -> Source.make ~path:mi.Module.resolved_source ())
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
      let%lwt frames = Ocaml_debug_agent.stack_frames agent in
      let%lwt stack_frames =
        frames
        |> Array.to_list
        |> Lwt_list.map_s (fun fr ->
               let module_ = Stack_frame.module_ fr in
               let source =
                 Source.(make ~path:module_.Module.resolved_source ())
               in
               let frame =
                 let loc = Stack_frame.loc fr in
                 Dap_stack_frame.(
                   make ~id:fr.index ~name:(Stack_frame.defname fr)
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
          make ~stack_frames ~total_frames:(Some (Array.length frames)) ()));
  Debug_rpc.set_command_handler rpc
    (module Scopes_command)
    (fun arg ->
      let frame_index = arg.frame_id in
      let%lwt frames = Ocaml_debug_agent.stack_frames agent in
      let frame = frames.(frame_index) in
      let scopes = frame.scopes |> List.map (fun obj ->
        Scope.make ~name:obj.name ~variables_reference:obj.id ~expensive:true ()
      ) in
      Lwt.return Scopes_command.Result.(make ~scopes ()));
  Lwt.join [ process_status_changes () ]
