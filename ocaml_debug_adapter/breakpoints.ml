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
    val trans_pos : [`Adapter_to_client | `Client_to_adapter] -> int * int -> int * int
    val source_by_modname : (string, Source.t) Hashtbl.t
    val user_source_by_modname : (string, Source.t) Hashtbl.t
  end) = struct

  include Args

  module Int_set = BatSet.Make (struct
      type t = int
      let compare a b = a - b
    end)

  let source_digest_tbl = (Hashtbl.create 0 : (string, Digest.t) Hashtbl.t)
  let user_source_digest_tbl = (Hashtbl.create 0 : (string, Digest.t) Hashtbl.t)
  let bppcs_by_modname = (Hashtbl.create 0 : (string, Int_set.t) Hashtbl.t)

  let has_breakpoint_at (rep : Debug_conn.report) =
    let ev = Symbols.event_at_pc symbols rep.rep_program_pointer in
    let bppcs = try Hashtbl.find bppcs_by_modname ev.ev_module with Not_found -> Int_set.empty in
    Int_set.mem ev.ev_pos bppcs


  let digest_of tbl path =
    try Hashtbl.find tbl path
    with Not_found -> (
        let digest = Digest.file path in
        Hashtbl.replace tbl path digest;
        digest
      )

  let is_same_source source1 source2 =
    match source1, source2 with
    | {Source.path = Some path1; _}, Some {Source.path = Some path2; _} -> (
        let digest1 = digest_of source_digest_tbl path1 in
        let digest2 = digest_of source_digest_tbl path2 in
        Lwt.return (Digest.equal digest1 digest2)
      )
    | _ -> Lwt.return_false

  let set_breakpoints_command (args : Set_breakpoints_command.Request.Arguments.t) =
    let path = BatOption.get args.source.path in
    let modname = Symbols.path_to_modname path in
    let bp_events = List.map (fun (bp : Source_breakpoint.t) ->
      let pos = (bp.line, BatOption.default 0 bp.column) |> trans_pos `Client_to_adapter in
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
    let%lwt breakpoints = Lwt_list.map_s (fun ev ->
      match ev with
      | Some ev -> (
          let (line, column) = Symbols.line_column_of_event ev |> trans_pos `Adapter_to_client in
          let source = BatHashtbl.find_option source_by_modname modname in
          let%lwt same = is_same_source args.source source in
          if same then (
            Hashtbl.replace user_source_by_modname ev.ev_module args.source;
          );
          let source = if same then Some args.source else source in
          Lwt.return Breakpoint.(
            make ~id:(Some ev.ev_pos) ~verified:true
              ~source ~line:(Some line) ~column:(Some column) ()
          )
        )
      | None -> Lwt.return Breakpoint.(make ~verified:false ())
    ) bp_events in
    Lwt.return_ok Set_breakpoints_command.Response.Body.({ breakpoints })

  let set_exception_breakpoints_command _ = Lwt.return_error ("Not supported", None)
  let set_function_breakpoints_command _ = Lwt.return_error ("Not supported", None)

  let () =
    Lwt_util.async (fun () ->
      BatHashtbl.keys symbols.event_by_pc
      |> BatEnum.fold (fun wait pc ->
        let%lwt () = wait in
        Debug_conn.set_event conn pc
      ) Lwt.return_unit;%lwt
      Rpc.emit_event rpc (module Initialized_event) ()
    )
end
