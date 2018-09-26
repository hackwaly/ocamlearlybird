open Debug_adapter_protocol
open Debug_protocol
open Debug_protocol_ex
open Signatures

module Make (Args : sig
    val rpc : Rpc.t
    val replace_agent : (module AGENT) -> unit
    val init_args : Initialize_command.Request.Arguments.t
    val caps : Capabilities.t
    val launch_args : Launch_command.Request.Arguments.t
    val proc : Agent_launched.launched_process
    val symbols : Symbols.t
    val conn : Debug_conn.t
    val pid : int
    val source_by_modname : (string, Source.t) Hashtbl.t
  end) = struct

  include Args

  module Int_set = BatSet.Make (struct
      type t = int
      let compare a b = a - b
    end)

  let bppcs_by_modname = (Hashtbl.create 0 : (string, Int_set.t) Hashtbl.t)

  let has_breakpoint_at (rep : Debug_conn.report) = 
    let ev = Symbols.event_at_pc symbols rep.rep_program_pointer in
    let bppcs = try Hashtbl.find bppcs_by_modname ev.ev_module with Not_found -> Int_set.empty in
    Int_set.mem ev.ev_pos bppcs

  let set_breakpoints_command (args : Set_breakpoints_command.Request.Arguments.t) = 
    let path = BatOption.get args.source.path in
    let modname = Symbols.path_to_modname path in
    let bp_events = List.map (fun (bp : Source_breakpoint.t) ->
      let pos = (bp.line, BatOption.default 0 bp.column) in
      Symbols.find_event_opt_near_pos symbols modname pos
    ) args.breakpoints in
    let prev_bppcs = try Hashtbl.find bppcs_by_modname modname with Not_found -> Int_set.empty in
    let next_bppcs = (
      bp_events
      |> List.filter BatOption.is_some
      |> List.map BatOption.get
      |> List.map (fun ev -> ev.Instruct.ev_pos)
      |> Int_set.of_list
    ) in
    Hashtbl.replace bppcs_by_modname modname next_bppcs;
    Lwt_list.iter_s (fun pc ->
      Debug_conn.reset_instruction conn pc
    ) Int_set.(diff prev_bppcs next_bppcs |> to_list);%lwt
    Lwt_list.iter_s (fun pc ->
      Debug_conn.set_breakpoint conn pc
    ) Int_set.(diff next_bppcs prev_bppcs |> to_list);%lwt
    let breakpoints = List.map (function
      | Some ev -> (
          let (line, column) = Symbols.line_column_of_event ev in
          Breakpoint.({
            id = Some ev.ev_pos;
            verified = true;
            message = None;
            source = Some (Hashtbl.find source_by_modname modname);
            line = Some line;
            column = Some column;
            end_line = None;
            end_column = None;
          })
        )
      | None -> (
          Breakpoint.({
            id = None;
            verified = false;
            message = None;
            source = None;
            line = None;
            column = None;
            end_line = None;
            end_column = None;
          })
        )
    ) bp_events in
    Lwt.return_ok Set_breakpoints_command.Response.Body.({ breakpoints })

  let set_exception_breakpoints_command _ = Lwt.return_error ("Not supported", None)
  let set_function_breakpoints_command _ = Lwt.return_error ("Not supported", None)

  let () = 
    Lwt.async (fun () ->
      BatHashtbl.keys symbols.event_by_pc
      |> BatEnum.fold (fun wait pc ->
        let%lwt () = wait in
        Debug_conn.set_event conn pc
      ) Lwt.return_unit;%lwt
      Rpc.emit_event rpc (module Initialized_event) ()
    )
end
