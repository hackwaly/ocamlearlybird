module Log = Log
open Util
open Module_values

type pc = Pc.t = { frag : int; pos : int }

type protocol_version = Debugcom.protocol_version = OCaml_400 | OCaml_410

type options = {
  protocol_version : protocol_version; [@default OCaml_410]
  debug_socket : Lwt_unix.file_descr;
  symbols_file : string;
  yield_point : int; [@default 1024]
}
[@@deriving make]

type status =
  | Unstarted
  | Running
  | Stopped of { time : int64; breakpoint : bool }
  | Exited of { time : int64; uncaught_exc : bool }

type stopped_action = [ `Run | `Step_in | `Step_out | `Step_over | `Stop ]

type action = [ stopped_action | `Pause | `Wakeup ]

type t = {
  options : options;
  status_s : status Lwt_react.S.t;
  set_status : status -> unit;
  action_e : action Lwt_react.E.t;
  emit_action : action -> unit;
  symbols : Symbols.t;
  breakpoints : Breakpoints.t;
  mutable pendings : (Debugcom.conn -> unit Lwt.t) list;
  ready : unit Lwt.t;
  ready_u : unit Lwt.u;
}

module Module = Module
module Event = Event
module Frame = Frame
module Value = Value

let ready agent = agent.ready

let symbols_did_update_event agent = Symbols.did_update_event agent.symbols

let to_seq_modules agent = Symbols.to_seq_modules agent.symbols

let find_module_by_source agent source =
  Symbols.find_module_by_source agent.symbols source

let find_module agent id = Symbols.find_module agent.symbols id

let find_event agent pc = Symbols.find_event agent.symbols pc

let is_running agent =
  match agent.status_s |> Lwt_react.S.value with Running -> true | _ -> false

let status_signal agent = agent.status_s

let create options =
  let status_s, set_status = React.S.create Unstarted in
  let action_e, emit_action = Lwt_react.E.create () in
  let breakpoints = Breakpoints.create () in
  let symbols = Symbols.create () in
  let ready, ready_u = Lwt.wait () in
  let agent =
    {
      options;
      status_s;
      set_status;
      action_e;
      emit_action;
      symbols;
      breakpoints;
      pendings = [];
      ready;
      ready_u;
    }
  in
  agent

let set_breakpoint agent pc =
  Breakpoints.set agent.breakpoints pc;
  agent.emit_action `Wakeup

let remove_breakpoint agent pc =
  Breakpoints.remove agent.breakpoints pc;
  agent.emit_action `Wakeup

let exec_in_loop agent f =
  let promise, resolver = Lwt.task () in
  agent.pendings <-
    (fun conn ->
      try%lwt
        let%lwt r = conn#lock f in
        Lwt.wakeup_later resolver r;
        Lwt.return ()
      with e ->
        (* TODO: Find a way to make Lwt delay backtrace *)
        Log.debug (fun m -> m "%s" (Printexc.get_backtrace ()));%lwt
        Lwt.wakeup_later_exn resolver e;
        Lwt.return ())
    :: agent.pendings;
  agent.emit_action `Wakeup;
  promise

let initial_frame agent =
  exec_in_loop agent (fun conn -> Debugcom.initial_frame conn)

let up_frame agent frame =
  exec_in_loop agent (fun conn -> Debugcom.up_frame conn frame)

let set_frame agent frame =
  exec_in_loop agent (fun conn -> Debugcom.set_frame conn frame)

let run agent = agent.emit_action `Run

let step_in agent = agent.emit_action `Step_in

let step_out agent = agent.emit_action `Step_out

let step_over agent = agent.emit_action `Step_over

let pause agent = agent.emit_action `Pause

let stop agent = agent.emit_action `Stop

let start agent =
  let%lwt fd, _ = Lwt_unix.accept agent.options.debug_socket in
  let conn =
    Debugcom.make_conn agent.options.protocol_version agent.symbols fd
  in
  let%lwt pid = Debugcom.get_pid conn in
  ignore pid;
  let sync () =
    let flush_pendings () =
      while%lwt agent.pendings <> [] do
        let pendings = List.rev agent.pendings in
        agent.pendings <- [];
        pendings |> Lwt_list.iter_s (fun f -> f conn)
      done
    in
    Symbols.commit agent.symbols (Debugcom.set_event conn)
      (Debugcom.reset_instr conn);%lwt
    Breakpoints.commit agent.breakpoints (Debugcom.set_breakpoint conn)
      (fun pc ->
        Debugcom.reset_instr conn pc;%lwt
        Debugcom.set_event conn pc);%lwt
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
  let execute =
    let temporary_trap_barrier_and_breakpoint = ref None in
    let check_met_temporary_trap_barrier_and_breakpoint report =
      match temporary_trap_barrier_and_breakpoint.contents with
      | None -> false
      | Some (stack_pos, pc) ->
          report.Debugcom.rep_stack_pointer = stack_pos
          && report.rep_program_pointer = pc
    in
    let rec stop_on_event report =
      let is_at_pseudo_event () =
        let event =
          Symbols.find_event agent.symbols report.Debugcom.rep_program_pointer
        in
        Debug_event.is_pseudo event.ev
      in
      let has_pc =
        match report.Debugcom.rep_type with
        | Exited | Uncaught_exc -> false
        | _ -> true
      in
      if has_pc && is_at_pseudo_event () then (
        Log.debug (fun m -> m "Pseudo event skipped");%lwt
        let%lwt report = Debugcom.go conn 1 in
        stop_on_event report )
      else Lwt.return report
    in
    let check_stop report0 =
      [%lwt assert (is_running agent)];%lwt
      sync ();%lwt
      let%lwt report = stop_on_event report0 in
      match report0.Debugcom.rep_type with
      | Breakpoint ->
          let met_temporary_trap_barrier_and_breakpoint =
            check_met_temporary_trap_barrier_and_breakpoint report0
          in
          if met_temporary_trap_barrier_and_breakpoint then
            Lwt.return
              (Some
                 ( report,
                   Stopped { time = report.rep_event_count; breakpoint = false }
                 ))
          else
            if%lwt
              Breakpoints.check agent.breakpoints report.rep_program_pointer
            then
              Lwt.return
                (Some
                   ( report,
                     Stopped
                       { time = report.rep_event_count; breakpoint = true } ))
            else Lwt.return None
      | Uncaught_exc ->
          Lwt.return
            (Some
               ( report,
                 Exited { time = report.rep_event_count; uncaught_exc = true }
               ))
      | Exited ->
          Lwt.return
            (Some
               ( report,
                 Exited { time = report.rep_event_count; uncaught_exc = false }
               ))
      | Trap -> (
          match temporary_trap_barrier_and_breakpoint.contents with
          | None -> [%lwt assert false]
          | Some _ ->
              let met_temporary_trap_barrier_and_breakpoint =
                check_met_temporary_trap_barrier_and_breakpoint report
              in
              if met_temporary_trap_barrier_and_breakpoint then
                Lwt.return
                  (Some
                     ( report,
                       Stopped
                         { time = report.rep_event_count; breakpoint = false }
                     ))
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
      let%lwt _, status = f () in
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
      let%lwt report = stop_on_event report in
      Lwt.return
        ( report,
          match report.rep_type with
          | Breakpoint ->
              Stopped { time = report.rep_event_count; breakpoint = true }
          | Event ->
              Stopped { time = report.rep_event_count; breakpoint = false }
          | Uncaught_exc ->
              Exited { time = report.rep_event_count; uncaught_exc = true }
          | Exited ->
              Exited { time = report.rep_event_count; uncaught_exc = false }
          | _ -> assert false )
    in
    let step_in = wrap_run internal_step_in in
    let internal_step_out () =
      let promise, resolver = Lwt.task () in
      Debugcom.exec_with_frame conn 1 (fun frame ->
          Lwt.wakeup_later resolver frame;
          Lwt.return ());%lwt
      let%lwt frame = promise in
      match frame with
      | None -> internal_run ()
      | Some frame ->
          let stack_pos = frame.stack_pos in
          let pc = Frame.pc frame in
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
      let%lwt stack_pos1, pc1 = Debugcom_basic.get_frame conn in
      let%lwt step_in_status = internal_step_in () in
      let%lwt stack_pos2, pc2 = Debugcom_basic.get_frame conn in
      let ev1 = find_event agent pc1 in
      let ev2 = find_event agent pc2 in
      (* tailcallopt case *)
      let is_tco () =
        if stack_pos2 - ev2.ev.ev_stacksize = stack_pos1 - ev1.ev.ev_stacksize
        then ev2.ev.ev_info = Event_function
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
  Symbols.load agent.symbols 0 agent.options.symbols_file;%lwt
  sync ();%lwt
  let%lwt report = Debugcom.go conn 1 in
  agent.set_status
    (Stopped { time = report.rep_event_count; breakpoint = false });
  Lwt.wakeup_later agent.ready_u ();
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

let global_variables agent frame =
  let globals = Symbols.globals agent.symbols frame.Frame.event.module_.frag in
  let%lwt env = Lazy.force frame.Frame.event.env in
  (* globals contains constructors not modules which we don't interested *)
  let globals =
    globals |> Ident.Map.to_seq
    |> Seq.filter_map (fun (id, pos) ->
           match env |> Env.find_module (Path.Pident id) with
           | _ -> Some (id, pos)
           | exception Not_found -> None)
  in
  exec_in_loop agent (fun conn ->
      let%lwt variables =
        globals |> List.of_seq
        |> Lwt_list.map_s (fun (id, pos) ->
               let%lwt rv = Debugcom.get_global conn pos in
               let value =
                 Module_value.Module
                   {
                     conn;
                     env;
                     rv;
                     path = Path.Pident id;
                     is_packaged = None;
                   }
               in
               Lwt.return (Ident.name id, value))
      in
      Lwt.return variables)

let frame_variables agent frame kind =
  exec_in_loop agent (fun conn ->
      let%lwt frame0 = Debugcom.initial_frame conn in
      ( Debugcom.set_frame conn frame;%lwt
        let%lwt env = Lazy.force frame.event.env in
        let tbl =
          match kind with
          | `Stack -> frame.event.ev.ev_compenv.ce_stack
          | `Heap -> frame.event.ev.ev_compenv.ce_heap
        in
        let get_rv pos =
          match kind with
          | `Stack -> Debugcom.get_local conn (frame.event.ev.ev_stacksize - pos)
          | `Heap -> Debugcom.get_environment conn pos
        in
        let iter f = tbl |> Ident.iter (fun id pos -> f (id, pos)) in
        let to_value (id, pos) =
          let%lwt rv = get_rv pos in
          match env |> Env.find_value (Path.Pident id) with
          | exception Not_found -> Lwt.return None
          | valdesc ->
              let ty = Ctype.correct_levels valdesc.val_type in
              let%lwt value = Value.adopt conn env ty rv in
              Lwt.return (Some (Ident.name id, value))
        in
        Iter.to_list iter
        |> List.fast_sort (Compare.by (fun (_, pos) -> pos))
        |> Lwt_list.filter_map_s to_value )
        [%finally Debugcom.set_frame conn frame0])

type frame_scope_kind = [ `Stack | `Heap ]

let list_variables agent frame kind =
  match kind with
  | `Global -> global_variables agent frame
  | #frame_scope_kind as kind -> frame_variables agent frame kind
