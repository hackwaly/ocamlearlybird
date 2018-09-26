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

  open Args

  module Ident_map = BatMap.Make (Ident)
  module Remote_value = Debug_conn.Remote_value

  type variable = {
    var_handle : int;
    var_name : string;
    var_value : string;
    var_vars : variable list Lwt.t Lazy.t option;
  }

  let stack = ref ([||] : (int * Instruct.debug_event) array)
  let site_id = ref 0
  let var_by_handle = (Hashtbl.create 0 : (int, variable) Hashtbl.t)
  let make_handle = 
    let next_handle = ref 1 in
    fun () ->
      let id = !next_handle in
      incr next_handle;
      id

  let walk_stack fn =
    let%lwt backup_stack_pos, _ = Debug_conn.get_frame conn in
    let wrap () =
      let%lwt stack_pos, pc = Debug_conn.initial_frame conn in
      if%lwt Lwt.return (stack_pos <> -1) then (
        let ev = Symbols.event_at_pc symbols pc in
        if%lwt fn (stack_pos, ev) then
          let rec walk_up (ev : Instruct.debug_event) =
            match%lwt Debug_conn.up_frame conn ev.ev_stacksize with
            | Some (stack_pos, pc) -> (
                match Symbols.event_at_pc symbols pc with
                | exception Not_found -> Lwt.return_unit
                | ev -> (
                    if%lwt fn (stack_pos, ev) then (
                      walk_up ev
                    )
                  )
              )
            | None -> Lwt.return_unit
          in walk_up ev
      )
    in
    (wrap ())[%finally Debug_conn.set_frame conn backup_stack_pos]

  let get_frames limit =
    let frames = ref [] in
    let count = ref 0 in
    walk_stack (fun (stack_pos, ev) ->
      incr count;
      frames := (stack_pos, ev) :: !frames;
      Lwt.return (
        match limit with
        | Some limit -> !count < limit
        | None -> true
      )
    );%lwt
    Lwt.return (!frames |> List.rev |> Array.of_list)

  let report ((rep : Debug_conn.report), guided) =
    let%lwt frames = get_frames None in
    stack := frames;
    incr site_id;
    if guided = `No_guide && rep.rep_type = Exited then (
      Rpc.emit_event rpc (module Terminated_event) { restart = `Assoc [] }
    ) else (
      let reason = match guided, rep.rep_type with
        | `Step, _ -> "step"
        | `No_guide, Event -> "step"
        | `No_guide, Trap_barrier -> "step"
        | `No_guide, Breakpoint -> "breakpoint"
        | `No_guide, Uncaught_exc -> "exception"
        | `No_guide, Exited -> assert false
      in
      Rpc.emit_event rpc (module Stopped_event) {
        reason;
        description = None;
        thread_id = Some 0;
        preserve_focus_hint = false;
        text = None;
        all_threads_stopped = true;
      }
    )

  let loaded_sources_command _ = 
    Lwt.return_ok Loaded_sources_command.Response.Body.{
      sources = BatHashtbl.values source_by_modname |> BatList.of_enum
    }

  let source_command _ = assert%lwt false

  let threads_command _ = 
    Lwt.return_ok Threads_command.Response.Body.{
      threads = [
        Thread.({
          id = 0;
          name = "main";
        })
      ]
    }

  let stack_trace_command _ = 
    let stack_frames =
      !stack
      |> Array.to_list
      |> List.mapi (fun i (_, ev) ->
        let line, column, end_line, end_column =
          if i = 0 then (
            let line, column = Symbols.line_column_of_event ev in
            line, column, None, None
          ) else (
            let (line, column) = Symbols.line_column_of_pos ev.ev_loc.loc_start in
            let (end_line, end_column) = Symbols.line_column_of_pos ev.ev_loc.loc_end in
            line, column, Some end_line, Some end_column
          )
        in
        Stack_frame.{
          id = i;
          name = "";
          source = Some (Hashtbl.find source_by_modname ev.ev_module);
          line; column; end_line; end_column;
          module_id = None;
          presentation_hint = None;
        }
      )
    in
    Lwt.return_ok Stack_trace_command.Response.Body.{
      stack_frames;
      total_frames = None;
    }

  let with_frame idx fn = 
    let cur_idx = ref 0 in
    let ret = ref None in
    walk_stack (fun (stack_pos, ev) -> 
      let at_frame = !cur_idx = idx in
      if%lwt Lwt.return at_frame then (
        let%lwt res = fn (stack_pos, ev) in
        ret := Some res;
        Lwt.return_unit
      );%lwt
      incr cur_idx;
      Lwt.return (not at_frame)
    );%lwt
    Lwt.return (BatOption.get !ret)

  let make_var name value get_vars =
    let handle, vars = match get_vars with
      | None -> 0, None
      | Some get_vars -> (
          let pin_site_id = !site_id in
          let handle = make_handle () in
          handle, Some (Lazy.from_fun (fun () ->
            if pin_site_id <> !site_id 
            then Lwt.return_nil
            else get_vars ()
          ))
        )
    in { 
      var_handle = handle;
      var_name = name;
      var_value = value;
      var_vars = vars;
    }

  let publish_var var = 
    Hashtbl.replace var_by_handle var.var_handle var

  let make_value_var name _ _ _ =
    make_var name "" None

  let make_scope_var frame_idx kind =
    let name = match kind with
      | `Stack -> "stack"
      | `Heap -> "heap"
    in
    make_var name "" (Some (fun () -> 
      with_frame frame_idx (fun (_, ev) -> 
        let env = Envaux.env_from_summary ev.ev_typenv ev.ev_typsubst in
        let tbl = match kind with `Stack -> ev.ev_compenv.ce_stack | `Heap -> ev.ev_compenv.ce_heap in
        let get_remote_value pos =
          match kind with
          | `Stack -> Remote_value.local conn (ev.ev_stacksize - pos)
          | `Heap -> Remote_value.from_environment conn pos
        in
        Ident.fold_name Ident_map.add tbl Ident_map.empty
        |> Ident_map.bindings
        |> Lwt_list.filter_map_s (fun (ident, _) ->
          let name = ident.Ident.name in
          match Env.lookup_value (Longident.Lident name) env with
          | exception Not_found -> Lwt.return_none
          | (_, valdesc) ->
            let ty = Ctype.correct_levels valdesc.Types.val_type in
            let pos = Ident.find_same ident tbl in
            let%lwt rv = get_remote_value pos in
            let var = make_value_var name env ty rv in
            Lwt.return_some (var :> variable)
        )
      )
    ))

  let scopes_command (args : Scopes_command.Request.Arguments.t) = 
    let make_scope_by_var var =  Scope.(
      make ~name:var.var_name ~variables_reference:var.var_handle ~expensive:true ()
    ) in
    let scope_vars = [
      make_scope_var args.frame_id `Stack;
      make_scope_var args.frame_id `Heap;
    ] in
    List.iter publish_var scope_vars;
    Lwt.return_ok Scopes_command.Response.Body.({
      scopes = List.map make_scope_by_var scope_vars
    })

  let variables_command (args : Variables_command.Request.Arguments.t) = 
    let var = Hashtbl.find var_by_handle args.variables_reference in
    let%lwt vars = Lazy.force (BatOption.get var.var_vars) in
    List.iter publish_var vars;
    let variables = vars |> List.map (fun var -> Variable.(
      make ~name:var.var_name ~value:var.var_value ~variables_reference:var.var_handle ()
    )) in
    Lwt.return_ok Variables_command.Response.Body.{ variables }

  let set_variable_command _ = Lwt.return_error ("Not supported", None)
  let set_expression_command _ = Lwt.return_error ("Not supported", None)
  let completions_command _ = Lwt.return_error ("Not supported", None)
end
