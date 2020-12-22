open Debug_protocol_ex
open Ocaml_debug_agent
module Dap_breakpoint = Debug_protocol_ex.Breakpoint
module Dap_stack_frame = Debug_protocol_ex.Stack_frame

type breakpoint_desc = {
  id : int;
  source : Source.t;
  src_bp : Source_breakpoint.t;
  mutable resolved : (Module.t * Instruct.debug_event) option;
}

module Breakpoint_desc = struct
  type t = breakpoint_desc

  let compare = Compare.by ~cmp:CCInt.compare (fun it -> it.id)
end

module Breakpoint_desc_set = CCSet.Make (Breakpoint_desc)
module String_to_multi_breakpoint_desc =
  CCMultiMap.Make (CCString) (Breakpoint_desc)

let run ~launch_args ~terminate ~agent rpc =
  ignore launch_args;
  ignore terminate;
  Lwt.pause ();%lwt
  let alloc_breakpoint_id = Unique_id.make_alloc () in
  let module_breakpoints_map = ref String_to_multi_breakpoint_desc.empty in
  let unresolved_breakpoints = ref Breakpoint_desc_set.empty in
  let to_dap_breakpoint desc =
    if desc.resolved |> Option.is_some then
      let is_line_brekpoint = desc.src_bp.column |> Option.is_none in
      let m, ev = desc.resolved |> Option.get in
      let lexing_pos = match ev.Instruct.ev_kind with
        | Event_before -> ev.ev_loc.Location.loc_start
        | Event_after _ -> ev.ev_loc.Location.loc_end
        | _ -> ev.ev_loc.Location.loc_start
      in
      Dap_breakpoint.(
        make ~id:(Some desc.id) ~verified:true
          ~source:
            (Some Source.(make ~path:(Some (m.resolved_source |> Option.get)) ()))
          ~line:
            (Some
               ( if is_line_brekpoint then desc.src_bp.line
               else lexing_pos.pos_lnum ))
          ~column:
            ( if is_line_brekpoint then None
            else Some (lexing_pos.pos_cnum - lexing_pos.pos_bol) )
          ())
    else Dap_breakpoint.make ~id:(Some desc.id) ~verified:false ()
  in
  let resolve_breakpoint desc =
    try%lwt
      let%lwt module_ =
        Ocaml_debug_agent.find_module_by_source agent
          (desc.source.path |> Option.get)
      in
      let%lwt event =
        Module.find_event module_ desc.src_bp.line
          (desc.src_bp.column |> Option.value ~default:0)
      in
      desc.resolved <- Some (module_, event);
      Ocaml_debug_agent.set_breakpoint agent
        { frag = module_.frag; pos = event.ev_pos };
      Lwt.return ()
    with
    | Not_found ->
        unresolved_breakpoints :=
          !unresolved_breakpoints |> Breakpoint_desc_set.add desc;
        Lwt.return ()
  in
  let resolve_breakpoints () =
    let symbols_updated_stream =
      Ocaml_debug_agent.symbols_updated_event agent |> Lwt_react.E.to_stream
    in
    while%lwt true do
      Log.debug (fun m ->
          m "unresolved_breakpoints %d"
            (Breakpoint_desc_set.cardinal !unresolved_breakpoints));%lwt
      !unresolved_breakpoints |> Breakpoint_desc_set.to_seq
      |> Lwt_util.iter_seq_s resolve_breakpoint;%lwt
      let resolved_breakpoints =
        !unresolved_breakpoints
        |> Breakpoint_desc_set.filter (fun desc ->
               desc.resolved |> Option.is_some)
      in
      let send_breakpoint_event desc =
        Debug_rpc.send_event rpc
          (module Breakpoint_event)
          Breakpoint_event.Payload.(
            make ~reason:Reason.Changed ~breakpoint:(to_dap_breakpoint desc))
      in
      Log.debug (fun m ->
          m "resolved_breakpoints %d"
            (Breakpoint_desc_set.cardinal resolved_breakpoints));%lwt
      resolved_breakpoints |> Breakpoint_desc_set.to_seq
      |> Lwt_util.iter_seq_s send_breakpoint_event;%lwt
      unresolved_breakpoints :=
        !unresolved_breakpoints
        |> Breakpoint_desc_set.filter (fun desc ->
               desc.resolved |> Option.is_none);
      Lwt_stream.next symbols_updated_stream;%lwt
      Log.debug (fun m -> m "symbols updated")
    done
  in
  Debug_rpc.set_command_handler rpc
    (module Set_breakpoints_command)
    (fun args ->
      let source_path = args.source.path |> Option.get in
      let make_desc src_bp =
        let id = alloc_breakpoint_id () in
        { id; source = args.source; src_bp; resolved = None }
      in
      let remove_breakpoint desc =
        if%lwt Lwt.return (Option.is_some desc.resolved) then (
          let module_, event = desc.resolved |> Option.get in
          Ocaml_debug_agent.remove_breakpoint agent
            { frag = module_.frag; pos = event.ev_pos };
          desc.resolved <- None;
          Lwt.return () );%lwt

        module_breakpoints_map :=
          String_to_multi_breakpoint_desc.remove !module_breakpoints_map
            source_path desc;

        unresolved_breakpoints :=
          !unresolved_breakpoints |> Breakpoint_desc_set.remove desc;
        Lwt.return ()
      in
      let prev =
        String_to_multi_breakpoint_desc.find !module_breakpoints_map source_path
      in
      Lwt_list.iter_s remove_breakpoint prev;%lwt
      module_breakpoints_map :=
        String_to_multi_breakpoint_desc.remove_all !module_breakpoints_map
          source_path;
      let next =
        args.breakpoints |> Option.value ~default:[] |> List.map make_desc
      in
      next
      |> List.iter (fun desc ->
             module_breakpoints_map :=
               String_to_multi_breakpoint_desc.add !module_breakpoints_map
                 source_path desc);
      next |> Lwt_list.iter_s resolve_breakpoint;%lwt
      let breakpoints = next |> List.map to_dap_breakpoint in
      Lwt.return Set_breakpoints_command.Result.(make ~breakpoints ()));
  Debug_rpc.set_command_handler rpc
    (module Set_exception_breakpoints_command)
    (fun _ -> Lwt.return_unit);
  Lwt.join [ resolve_breakpoints () ]
