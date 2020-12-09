open Debug_protocol_ex

let src = Logs.Src.create "earlybird.Debug_agent"
module Log = (val Logs_lwt.src_log src : Logs_lwt.LOG)

type t = {
  symbols : Symbols.t;
  conn : Debug_conn.t;
  time_slice : int;
  stopped_e : Debug_conn.report React.E.t;
  emit_stopped : Debug_conn.report -> unit;
  pause_flag_s : bool React.S.t;
  set_pause_flag : bool -> unit;
  wakeup_e : unit React.E.t;
  emit_wakeup : unit -> unit;
  mutable pendings : (unit -> unit Lwt.t) list;
}

let create ~symbols  ~conn ?(time_slice=1024) () =
  let (stopped_e, emit_stopped) = React.E.create () in
  let (pause_flag_s, set_pause_flag) = React.S.create false in
  let (wakeup_e, emit_wakeup) = React.E.create () in
  { symbols;
    conn;
    time_slice;
    stopped_e;
    emit_stopped;
    pause_flag_s;
    set_pause_flag;
    pendings = [];
    wakeup_e;
    emit_wakeup }

let load agent =
  let%lwt pid = Debug_conn.initial agent.conn in
  Log.info (fun m -> m "Debuggee pid: %d" pid);%lwt
  (* let setup_events () =
    Hashtbl.to_seq_keys agent.symbols.event_by_pc
    |> Seq.fold_left (fun wait pc ->
      let%lwt () = wait in
      Debug_conn.set_event agent.conn pc
    ) Lwt.return_unit
  in
  setup_events ();%lwt *)
  Lwt.return_unit

let start agent =
  let break = ref false in
  let flush_pendings () =
    while%lwt agent.pendings <> [] do
      let pendings = List.rev agent.pendings in
      agent.pendings <- [];
      pendings |> Lwt_list.iter_s (fun p -> p ());
    done
  in
  let rec next () =
    let%lwt report = Debug_conn.go agent.conn agent.time_slice in
    match report.rep_type with
    | Exited -> (
      Lwt.return report;
    )
    | _ -> (
      if React.S.value agent.pause_flag_s then (
        Lwt.return report
      ) else (
        next ()
      )
    )
  in
  while%lwt not !break do
    flush_pendings ();%lwt
    let%lwt skip_run =
      if React.S.value agent.pause_flag_s then (
        let%lwt is_wakeup = Lwt_react.E.next (
          Lwt_react.E.select [
            agent.pause_flag_s
            |> React.S.changes
            |> React.E.fmap (fun pause ->
              if not pause then Some true else None
            );
            agent.wakeup_e |> React.E.map (fun () -> true);
          ]
        ) in
        Log.debug (fun m -> m "is_wakeup: %b" is_wakeup);%lwt
        Lwt.return is_wakeup
      ) else (
        Lwt.return_false
      )
    in
    if not skip_run then (
      let%lwt report = next () in
      agent.emit_stopped report;
      let () = match report.rep_type with
        | Exited -> break := true;
        | _ -> ()
      in
      Lwt.return_unit
    ) else Lwt.return_unit;%lwt
    Lwt.return_unit
  done;%lwt
  Lwt.return_unit

let stopped_event agent =
  agent.stopped_e

let loaded_sources agent =
  agent.symbols.module_info_tbl
  |> Hashtbl.to_seq_values
  |> Seq.filter_map (fun mi ->
    Some (Source.make ~path:(mi.Symbols.source) ())
  )
  |> List.of_seq

let push_pending agent p =
  agent.pendings <- p :: agent.pendings;
  agent.emit_wakeup ()

let debug_set_breakpoint agent =
  push_pending agent (fun () ->
    Lwt.return_unit
  )

let debug_run agent =
  agent.set_pause_flag false

let debug_pause agent =
  agent.set_pause_flag true
