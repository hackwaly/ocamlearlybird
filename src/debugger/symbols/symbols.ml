type t = {
  event_by_pc : (Pc.t, Debuginfo.event) Hashtbl.t;
  events_commit_queue : (Pc.t, unit) Hashtbl.t;
  events_committed : (Pc.t, unit) Hashtbl.t;
  module_by_id : (string, Debuginfo.module_) Hashtbl.t;
  module_by_source : (string, Debuginfo.module_) Hashtbl.t;
  did_update_e : unit Lwt_react.E.t;
  emit_did_update : unit -> unit;
}

let create () =
  let did_update_e, emit_did_update = Lwt_react.E.create () in
  {
    event_by_pc = Hashtbl.create 0;
    events_commit_queue = Hashtbl.create 0;
    events_committed = Hashtbl.create 0;
    module_by_id = Hashtbl.create 0;
    module_by_source = Hashtbl.create 0;
    did_update_e;
    emit_did_update;
  }

let did_update_event t = t.did_update_e

let to_seq_modules t = t.module_by_id |> Hashtbl.to_seq_values

let find_module_by_source t source =
  Hashtbl.find t.module_by_source source

let find_module t id =
  Hashtbl.find t.module_by_id id

let find_event t pc =
  Hashtbl.find t.event_by_pc pc

let load t frag file =
  let%lwt modules = Debuginfo.load frag file in
  let add_event (event : Debuginfo.event) =
    Hashtbl.replace t.event_by_pc (Event.pc event) event;
    Hashtbl.replace t.events_commit_queue (Event.pc event) ()
  in
  let add_module (module_ : Debuginfo.module_) =
    Hashtbl.replace t.module_by_id module_.id module_;
    module_.events
    |> CCArray.to_iter
    |> Iter.iter add_event;
    match module_.source with
    | Some source ->
        Hashtbl.replace t.module_by_source source module_;
        Lwt.return ()
    | None -> Lwt.return ()
  in
  modules |> Lwt_list.iter_s add_module;%lwt
  t.emit_did_update ();
  Lwt.return ()

let commit t set clear =
	let to_set =
  	t.events_commit_queue
    |> Hashtbl.to_seq_keys
    |> Seq.filter (fun pc ->
      Hashtbl.mem t.event_by_pc pc &&
      not (Hashtbl.mem t.events_committed pc))
  in
  let to_clear =
  	t.events_commit_queue
    |> Hashtbl.to_seq_keys
    |> Seq.filter (fun pc ->
      not (Hashtbl.mem t.event_by_pc pc) &&
      Hashtbl.mem t.events_committed pc)
  in
  Log.debug (fun m -> m "%s" ([%show : Pc.t list] (to_set |> List.of_seq)) );%lwt
  to_set |> Lwt_util.iter_seq_s set;%lwt
  to_clear |> Lwt_util.iter_seq_s clear;%lwt
  Hashtbl.reset t.events_commit_queue;
  Lwt.return ()
