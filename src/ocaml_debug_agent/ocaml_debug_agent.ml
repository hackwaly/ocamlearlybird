include Inspect_types
module Log = Log

type pc = Pc.t = { frag : int; pos : int }

type remote_debugger_version = Debugcom.remote_debugger_version =
  | OCaml_400
  | OCaml_410

type options = {
  remote_debugger_version : remote_debugger_version; [@default OCaml_410]
  debug_socket : Lwt_unix.file_descr;
  symbols_file : string;
  yield_point : int; [@default 1024]
}
[@@deriving make]

type status =
  | Entry
  | Running
  | Stopped of { breakpoint : bool }
  | Exited of { uncaught_exc : bool }

type stopped_action = [ `Run | `Step_in | `Step_out | `Step_over | `Stop ]

type action = [ stopped_action | `Pause | `Wakeup ]

type t = {
  options : options;
  status_s : status Lwt_react.S.t;
  set_status : status -> unit;
  action_e : action Lwt_react.E.t;
  emit_action : action -> unit;
  (* #region Symbols *)
  event_by_pc : (Pc.t, Debug_info.event) Hashtbl.t;
  events_commit_queue : (Pc.t, unit) Hashtbl.t;
  events_committed : (Pc.t, unit) Hashtbl.t;
  module_by_id : (string, Debug_info.module_) Hashtbl.t;
  module_by_source : (string, Debug_info.module_) Hashtbl.t;
  symbols_updated_e : unit Lwt_react.E.t;
  emit_symbols_updated : unit -> unit;
  (* #endregion *)
  breakpoints : Breakpoints.t;
  mutable pendings : (Debugcom.conn -> unit Lwt.t) list;
  alloc_obj_id : unit -> int;
  mutable scene : scene option;
}

module Module = Module
module Event = Event
module Stack_frame = Stack_frame

let create options =
  let status_s, set_status = React.S.create Entry in
  let action_e, emit_action = Lwt_react.E.create () in
  let symbols_updated_e, emit_symbols_updated = Lwt_react.E.create () in
  let breakpoints = Breakpoints.create () in
  {
    options;
    status_s;
    set_status;
    action_e;
    emit_action;
    event_by_pc = Hashtbl.create 0;
    events_commit_queue = Hashtbl.create 0;
    events_committed = Hashtbl.create 0;
    module_by_id = Hashtbl.create 0;
    module_by_source = Hashtbl.create 0;
    symbols_updated_e;
    emit_symbols_updated;
    breakpoints;
    pendings = [];
    alloc_obj_id = Unique_id.make_alloc ();
    scene = None;
  }

let symbols_updated_event agent = agent.symbols_updated_e

let to_seq_modules agent = agent.module_by_id |> Hashtbl.to_seq_values

let find_module_by_source agent source =
  Hashtbl.find agent.module_by_source source |> Lwt.return

let find_module agent id =
  Hashtbl.find agent.module_by_id id

let find_event agent pc =
  Hashtbl.find agent.event_by_pc pc

let is_running agent =
  match agent.status_s |> Lwt_react.S.value with Running -> true | _ -> false

let status_signal agent = agent.status_s

let set_breakpoint agent pc =
  Breakpoints.set agent.breakpoints pc;
  agent.emit_action `Wakeup

let remove_breakpoint agent pc =
  Breakpoints.remove agent.breakpoints pc;
  agent.emit_action `Wakeup

let run agent = agent.emit_action `Run

let step_in agent = agent.emit_action `Step_in

let step_out agent = agent.emit_action `Step_out

let step_over agent = agent.emit_action `Step_over

let pause agent = agent.emit_action `Pause

let stop agent = agent.emit_action `Stop

let stack_frames agent =
  Lwt.return (match agent.scene with None -> [||] | Some scene -> scene.frames)

let exec_in_loop agent f =
  Log.debug (fun m -> m "exec_in_loop 1");%lwt
  let promise, resolver = Lwt.task () in
  agent.pendings <-
    (fun conn ->
      Log.debug (fun m -> m "exec_in_loop 2");%lwt
      match%lwt f conn with
      | result ->
          Lwt.wakeup_later resolver result;
          Lwt.return ()
      | exception exc ->
          Lwt.wakeup_later_exn resolver exc;
          Lwt.return ())
    :: agent.pendings;
  agent.emit_action `Wakeup;
  Log.debug (fun m -> m "exec_in_loop 3");%lwt
  promise

let find_obj agent id =
  match agent.scene with
  | None -> raise Not_found
  | Some scene -> Hashtbl.find scene.obj_tbl id

let exec_with_frame agent conn index f =
  Log.debug (fun m -> m "exec_with_frame");%lwt
  assert (index >= 0);
  let rec walk cur (stack_pos, pc) =
    Log.debug (fun m -> m "exec_with_frame cur:%d" cur);%lwt
    let ev = find_event agent pc in
    if cur = index then Lwt.return (Some (stack_pos, pc, ev))
    else
      match%lwt Debugcom.up_frame conn ev.ev.Instruct.ev_stacksize with
      | None -> Lwt.return None
      | Some (stack_pos, pc) -> walk (cur + 1) (stack_pos, pc)
  in
  let%lwt stack_pos, pc = Debugcom.initial_frame conn in
  let%lwt frame = walk 0 (stack_pos, pc) in
  (f frame) [%finally Debugcom.set_frame conn stack_pos]

let undef_lazy = Lazy.from_fun (fun () -> assert false)

let rec make_obj agent ~name ~value ?(structured = false)
    ?(list_members = fun _ -> Lwt.return_nil) () =
  match agent.scene with
  | Some scene ->
      let id = agent.alloc_obj_id () in
      let obj = { id; name; value; structured; members = undef_lazy } in
      obj.members <- Lazy.from_fun (fun () -> list_members obj);
      Hashtbl.replace scene.obj_tbl id obj;
      obj
  | None -> assert false

and list_scope_obj agent obj =
  let[@warning "-8"] (Scope { scene_id; index; kind; _ }) =
    (obj.value [@warning "+8"])
  in
  match agent.scene with
  | Some scene when scene.report.rep_event_count = scene_id ->
      exec_in_loop agent (fun conn ->
          let frame = scene.frames.(index) in
          let event = frame.event in
          match%lwt Lazy.force frame.env with
          | exception _ ->
              Log.debug (fun m -> m "no env");%lwt
              Lwt.return []
          | env ->
              Log.debug (fun m -> m "has env");%lwt
              let ident_tbl =
                match kind with
                | `Stack -> event.ev.ev_compenv.ce_stack
                | `Heap -> event.ev.ev_compenv.ce_heap
                | `Global -> assert false
              in
              let iter_bindings f =
                Ident.iter (fun id index -> f (id, index)) ident_tbl
              in
              let find_ty env path =
                let val_desc = env |> Env.find_value path in
                let ty = Ctype.correct_levels val_desc.Types.val_type in
                ty
              in
              let to_obj (ident, pos) =
                Log.debug (fun m -> m "local_to_obj: %s" (Ident.name ident));%lwt
                match find_ty env (Path.Pident ident) with
                | exception Not_found -> Lwt.return None
                | ty ->
                    let%lwt rv =
                      match kind with
                      | `Stack ->
                          Debugcom.get_local conn (event.ev.ev_stacksize - pos)
                      | `Heap -> Debugcom.get_environment conn pos
                      | `Global -> [%lwt assert false]
                    in
                    let%lwt value =
                      Inspect.get_value (find_event agent) conn env rv ty
                    in
                    let obj =
                      make_obj agent ~name:(Ident.name ident) ~value ()
                    in
                    Lwt.return (Some obj)
              in
              let%lwt objs =
                exec_with_frame agent conn index (fun _ ->
                    iter_bindings |> Iter.to_list
                    |> Lwt_list.filter_map_s to_obj)
              in
              Log.debug (fun m -> m "list_scope_obj 2");%lwt
              Lwt.return objs)
  | _ -> [%lwt assert false]

let load_debug_info frag agent file =
  let%lwt modules = Debug_info.load frag file in
  let add_event (event : Debug_info.event) =
    Hashtbl.replace agent.event_by_pc (Event.pc event) event;
    Hashtbl.replace agent.events_commit_queue (Event.pc event) ()
  in
  let add_module (module_ : Debug_info.module_) =
    Hashtbl.replace agent.module_by_id module_.id module_;
    module_.events
    |> CCArray.to_iter
    |> Iter.iter add_event;
    match module_.source with
    | Some source ->
        Hashtbl.replace agent.module_by_source source module_;
        Lwt.return ()
    | None -> Lwt.return ()
  in
  modules |> Lwt_list.iter_s add_module;%lwt
  Lwt.return ()

let commit_events agent conn =
  let commit_one pc =
    let committed = Hashtbl.mem agent.events_committed pc in
    if%lwt Lwt.return (not committed) then (
      Hashtbl.replace agent.events_committed pc ();
      Debugcom.set_event conn pc )
  in
  Log.debug (fun m -> m "commit_events start");%lwt
  agent.events_commit_queue |> Hashtbl.to_seq_keys |> Lwt_util.iter_seq_s commit_one;%lwt
  Hashtbl.reset agent.events_commit_queue;
  Log.debug (fun m -> m "commit_events end");%lwt
  Lwt.return ()

let start agent =
  let%lwt fd, _ = Lwt_unix.accept agent.options.debug_socket in
  let%lwt conn =
    Debugcom.connect agent.options.remote_debugger_version
      Lwt_io.(of_fd ~mode:input fd)
      Lwt_io.(of_fd ~mode:output fd)
  in
  let%lwt pid = Debugcom.get_pid conn in
  ignore pid;
  let flush_pendings () =
    while%lwt agent.pendings <> [] do
      let pendings = List.rev agent.pendings in
      agent.pendings <- [];
      pendings |> Lwt_list.iter_s (fun f -> f conn)
    done
  in
  let sync () =
    commit_events agent conn;%lwt
    Breakpoints.commit agent.breakpoints conn;%lwt
    flush_pendings ()
  in
  let wait_action () =
    Log.debug (fun m -> m "wait_action 1");%lwt
    let%lwt action =
      agent.action_e
      |> Lwt_react.E.fmap (fun action ->
             match action with
             | `Pause -> None
             | #stopped_action as x -> Some (Some x)
             | `Wakeup -> Some None)
      |> Lwt_react.E.once |> Lwt_react.E.to_stream |> Lwt_stream.next
    in
    Log.debug (fun m -> m "wait_action 2");%lwt
    Lwt.return action
  in
  let clear_scene agent = agent.scene <- None in
  let update_scene agent report =
    let obj_tbl = Hashtbl.create 0 in
    let%lwt frames =
      match report.Debugcom.rep_type with
      | Event | Breakpoint ->
          let%lwt curr_fr_sp, _ = Debugcom.get_frame conn in
          let make_frame index sp (pc : pc) =
            let event = find_event agent pc in
            let module_ = find_module agent event.ev.ev_module in
            {
              Stack_frame.index;
              stack_pos = sp;
              module_;
              event;
              scopes = [];
              env = event.env;
            }
          in
          let rec walk_up index stacksize frames =
            let index = index + 1 in
            match%lwt Debugcom.up_frame conn stacksize with
            | Some (sp, pc) ->
                let frame = make_frame index sp pc in
                walk_up index (Stack_frame.stacksize frame) (frame :: frames)
            | None -> Lwt.return frames
          in
          (let%lwt sp, pc = Debugcom.initial_frame conn in
           let intial_frame = make_frame 0 sp pc in
           let%lwt frames =
             walk_up 0 (Stack_frame.stacksize intial_frame) [ intial_frame ]
           in
           let frames = List.rev frames |> Array.of_list in
           Lwt.return frames)
            [%finally Debugcom.set_frame conn curr_fr_sp]
      | _ -> Lwt.return [||]
    in
    let scene = { report; frames; obj_tbl } in
    agent.scene <- Some scene;
    (* We delay set Stack_frame's `scopes` field. Because make_obj need agent's `scene` field to be set *)
    scene.frames
    |> Array.iter (fun (frame : Stack_frame.t) ->
           let make_scope_obj name kind =
             make_obj agent ~name
               ~value:
                 (Scope
                    {
                      scene_id = report.rep_event_count;
                      index = frame.index;
                      kind;
                    })
               ~structured:true () ~list_members:(list_scope_obj agent)
           in
           let scopes =
             [ make_scope_obj "Stack" `Stack; make_scope_obj "Heap" `Heap ]
           in
           frame.scopes <- scopes);
    Lwt.return ()
  in
  let execute =
    let temporary_trap_barrier_and_breakpoint = ref None in
    let check_met_temporary_trap_barrier_and_breakpoint report =
      match temporary_trap_barrier_and_breakpoint.contents with
      | None -> false
      | Some (stack_pos, pc) ->
          report.Debugcom.rep_stack_pointer = stack_pos
          && report.rep_program_pointer = pc
    in
    let check_stop report =
      [%lwt assert (is_running agent)];%lwt
      sync ();%lwt
      match report.Debugcom.rep_type with
      | Breakpoint ->
          let met_temporary_trap_barrier_and_breakpoint =
            check_met_temporary_trap_barrier_and_breakpoint report
          in
          if met_temporary_trap_barrier_and_breakpoint then
            Lwt.return (Some (report, Stopped { breakpoint = false }))
          else
            if%lwt
              Breakpoints.check agent.breakpoints report.rep_program_pointer
            then Lwt.return (Some (report, Stopped { breakpoint = true }))
            else Lwt.return None
      | Uncaught_exc ->
          Lwt.return (Some (report, Exited { uncaught_exc = true }))
      | Exited -> Lwt.return (Some (report, Exited { uncaught_exc = false }))
      | Trap -> (
          match temporary_trap_barrier_and_breakpoint.contents with
          | None -> [%lwt assert false]
          | Some _ ->
              let met_temporary_trap_barrier_and_breakpoint =
                check_met_temporary_trap_barrier_and_breakpoint report
              in
              if met_temporary_trap_barrier_and_breakpoint then
                Lwt.return (Some (report, Stopped { breakpoint = false }))
              else Lwt.return None )
      | _ -> Lwt.return None
    in
    let exec_with_trap_barrier stack_pos f =
      Debugcom.set_trap_barrier conn stack_pos;%lwt
      (f ()) [%finally Debugcom.set_trap_barrier conn 0]
    in
    let exec_with_temporary_breakpoint pc f =
      let already_has_bp = Breakpoints.is_commited agent.breakpoints pc in
      if already_has_bp then f ()
      else
        let cleanup () =
          Debugcom.reset_instr conn pc;%lwt
          Debugcom.set_event conn pc
        in
        Debugcom.set_breakpoint conn pc;%lwt
        (f ()) [%finally cleanup ()]
    in
    let wrap_run f () =
      agent.set_status Running;
      clear_scene agent;
      let%lwt report, status = f () in
      update_scene agent report;%lwt
      agent.set_status status;
      Lwt.return ()
    in
    let internal_run () =
      let rec loop () =
        let%lwt report = Debugcom.go conn agent.options.yield_point in
        match%lwt check_stop report with
        | Some status -> Lwt.return status
        | None -> loop ()
      in
      loop ()
    in
    let run = wrap_run internal_run in
    let internal_step_in () =
      let%lwt report = Debugcom.go conn 1 in
      Lwt.return
        ( report,
          match report.rep_type with
          | Breakpoint -> Stopped { breakpoint = true }
          | Event -> Stopped { breakpoint = false }
          | Uncaught_exc -> Exited { uncaught_exc = true }
          | Exited -> Exited { uncaught_exc = false }
          | _ -> assert false )
    in
    let step_in = wrap_run internal_step_in in
    let internal_step_out () =
      let promise, resolver = Lwt.task () in
      exec_with_frame agent conn 1 (fun frame ->
          Lwt.wakeup_later resolver frame;
          Lwt.return ());%lwt
      let%lwt frame = promise in
      match frame with
      | None -> internal_run ()
      | Some (stack_pos, pc, _) ->
          temporary_trap_barrier_and_breakpoint := Some (stack_pos, pc);
          let cleanup () =
            temporary_trap_barrier_and_breakpoint := None;
            Lwt.return ()
          in
          (exec_with_trap_barrier stack_pos (fun () ->
               exec_with_temporary_breakpoint pc (fun () ->
                   let rec loop () =
                     let%lwt report =
                       Debugcom.go conn agent.options.yield_point
                     in
                     match%lwt check_stop report with
                     | Some status -> Lwt.return status
                     | None -> loop ()
                   in
                   loop ())))
            [%finally cleanup ()]
    in
    let step_out = wrap_run internal_step_out in
    let internal_step_over () =
      let%lwt stack_pos1, pc1 = Debugcom.get_frame conn in
      let%lwt step_in_status = internal_step_in () in
      let%lwt stack_pos2, pc2 = Debugcom.get_frame conn in
      let ev1 = find_event agent pc1 in
      let ev2 = find_event agent pc2 in
      (* tailcallopt case *)
      let is_tco () =
        if stack_pos2 - ev2.ev.ev_stacksize = stack_pos1 - ev1.ev.ev_stacksize then
          ev2.ev.ev_info = Event_function
        else false
      in
      let is_entered () =
        stack_pos2 - ev2.ev.ev_stacksize > stack_pos1 - ev1.ev.ev_stacksize
      in
      if is_entered () || is_tco () then internal_step_out ()
      else Lwt.return step_in_status
    in
    let step_over = wrap_run internal_step_over in
    let stop () =
      Debugcom.stop conn;%lwt
      Lwt.fail Exit
    in
    function
    | `Run -> run ()
    | `Step_in -> step_in ()
    | `Step_out -> step_out ()
    | `Step_over -> step_over ()
    | `Stop -> stop ()
  in
  load_debug_info 0 agent agent.options.symbols_file;%lwt
  agent.emit_symbols_updated ();
  try%lwt
    while%lwt true do
      sync ();%lwt
      match%lwt wait_action () with
      | Some action ->
          execute action;%lwt
          if%lwt
            Lwt.return
              ( match agent.status_s |> Lwt_react.S.value with
              | Exited _ -> true
              | _ -> false )
          then Lwt.fail Exit
      | None -> Lwt.return ()
    done
  with Exit -> Lwt.return ()
