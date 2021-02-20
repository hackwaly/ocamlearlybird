(**
 * Copyright (C) 2021 Yuxiang Wen
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Ground
open Int64ops
open Instruct
open Frame
module PcSet_ = Set.Make (Ordered_type.Make_tuple2 (Int) (Int))
module IntMap_ = Map.Make (Int)

type pc = int * int

type 'a source_location = 'a Debug_types.source_location = {
  source : string;
  pos : int * int;
  end_ : 'a;
}

type source_position = unit source_location

type source_range = (int * int) source_location

type options = {
  debug_sock : Lwt_unix.file_descr;
  symbols_file : string;
  follow_fork_mode : [ `Fork_parent | `Fork_child ]; [@default `Fork_child]
  yield_steps : int; [@default Int.max_int]
  debug_filter : string -> bool;
}
[@@deriving make]

type reason = Entry | Step | Breakpoint | Pause | Exited | Uncaught_exc

type state = Running | Stopped of reason

type source_breakpoint = {
  bp_id : int;
  mutable bp_loc : source_position;
  mutable bp_resolved_loc : source_position;
  mutable bp_pc : pc option;
  mutable bp_active : bool;
  mutable bp_version : int;
  mutable bp_on_change : source_breakpoint -> unit Lwt.t;
}

type t = {
  opts : options;
  c : Controller.t;
  mutable interupt_flag : bool;
  state : state Lwt_react.S.t;
  set_state : state -> unit;
  mutable source_breakpoints : (int, source_breakpoint) Hashtbl.t;
  mutable breakpoints : PcSet_.t;
  mutable pc_to_bp : (pc, source_breakpoint) Hashtbl.t;
  mutable bp_used_symver : int;
  mutable bp_sync_requested : bool;
  mutable scene : Scene.t option;
}

type value =
  < to_short_string : string
  ; num_indexed : int
  ; num_named : int
  ; get_indexed : int -> value Lwt.t
  ; list_named : (string * value) list Lwt.t >

type frame = {
  index : int;
  name : string option;
  loc : source_range option;
  scopes : (string * value) list;
  _frame : Frame.t;
}

let init opts =
  let%lwt c =
    Controller.root ~debug_filter:opts.debug_filter opts.debug_sock
      opts.symbols_file
  in
  Controller.set_follow_fork_mode c opts.follow_fork_mode;%lwt
  let state, set_state = Lwt_react.S.create (Stopped Entry) in
  Lwt.return
    {
      opts;
      c;
      interupt_flag = false;
      state;
      set_state;
      source_breakpoints = Hashtbl.create 0;
      breakpoints = PcSet_.empty;
      pc_to_bp = Hashtbl.create 0;
      bp_used_symver = -1;
      bp_sync_requested = false;
      scene = None;
    }

let get_sources t =
  Symbols.to_source_modules_seq t.c.symbols
  |> Seq.map (fun it -> (it.Code_module.source |> Option.get).path)
  |> List.of_seq

let get_frames ?start ?count t =
  match t.scene with
  | None -> Lwt.return []
  | Some scene ->
      let repr_frame frame =
        let name =
          let open Option in
          let* event = frame.event in
          return event.ev_defname
        in
        {
          index = frame.index;
          name;
          loc = frame.loc;
          _frame = frame;
          scopes =
            (match frame.event with
            | Some _ ->
                [
                  ("Stack", Inspect.scope scene frame `Stack);
                  ("Heap", Inspect.scope scene frame `Heap);
                  ("Global", Inspect.scope scene frame `Global);
                ]
            | None -> []);
        }
      in
      let frames = Scene.frames scene |> Lwt_stream.map repr_frame in
      if start |> Option.is_some then
        frames |> Lwt_stream.njunk (start |> Option.get)
      else Lwt.return ();%lwt
      if count |> Option.is_some then
        frames |> Lwt_stream.nget (count |> Option.get)
      else frames |> Lwt_stream.to_list

let state t = t.state

let stop t =
  t.interupt_flag <- true;
  Lwt.async (fun () ->
      let stopped, stopped_u = Lwt.task () in
      Lwt_conn.atomic t.c.conn (fun _ ->
          Lwt.wakeup stopped_u ();
          Lwt.return ());%lwt
      stopped;%lwt
      Controller.stop ~gracefully:true t.c;%lwt
      t.set_state (Stopped Exited);
      Lwt.return ());
  Lwt.return ()

let stop_imm t = Controller.stop ~gracefully:false t.c

let pause t =
  t.interupt_flag <- true;
  Lwt.return ()

let rec _sync_breakpoints t =
  if t.bp_used_symver <> t.c.symbols.version then (
    t.source_breakpoints |> Hashtbl.to_seq_values |> Seq.iter (_resolve_bp t);
    t.bp_used_symver <- t.c.symbols.version);
  let to_set = PcSet_.diff t.breakpoints t.c.breakpoints in
  let to_unset = PcSet_.diff t.c.breakpoints t.breakpoints in
  let set_bp pc =
    Controller.set_breakpoint t.c pc;%lwt
    let bp = Hashtbl.find t.pc_to_bp pc in
    bp.bp_active <- true;
    bp.bp_version <- bp.bp_version + 1;
    bp.bp_on_change bp
  in
  let unset_bp pc =
    Controller.remove_breakpoint t.c pc;%lwt
    match Hashtbl.find_opt t.pc_to_bp pc with
    | Some bp ->
        bp.bp_active <- false;
        bp.bp_version <- bp.bp_version + 1;
        bp.bp_on_change bp
    | None -> Lwt.return ()
  in
  to_unset |> PcSet_.to_seq |> Lwt_seq.iter_s unset_bp;%lwt
  to_set |> PcSet_.to_seq |> Lwt_seq.iter_s set_bp;%lwt
  Lwt.return ()

and _schedule_sync_breakpoints t =
  if not t.bp_sync_requested then (
    t.bp_sync_requested <- true;
    Lwt.async (fun () ->
        t.bp_sync_requested <- false;
        Lwt.pause ();%lwt
        _sync_breakpoints t))

and _resolve_bp t bp =
  if t.c.symbols.debug_filter bp.bp_loc.source then
    match bp.bp_pc with
    | Some ((frag_num, _) as pc) ->
        if not (t.c.symbols.frags |> IntMap_.mem frag_num) then
          t.breakpoints <- t.breakpoints |> PcSet_.remove pc
    | None -> (
        try
          let module_ =
            t.c.symbols |> Symbols.find_source_module bp.bp_loc.source
          in
          let line, column = bp.bp_loc.pos in
          let event = Code_module.find_event module_ ~line ~column () in
          let pc = (module_.frag, event.ev_pos) in
          Hashtbl.replace t.pc_to_bp pc bp;
          bp.bp_pc <- Some pc;
          bp.bp_resolved_loc <-
            {
              bp.bp_loc with
              source = (module_.source |> Option.get).path;
              pos = Util.Debug_event.line_column event;
            };
          bp.bp_version <- bp.bp_version + 1;
          bp.bp_active <- t.c.breakpoints |> PcSet_.mem pc;
          t.breakpoints <- t.breakpoints |> PcSet_.add pc;
          _schedule_sync_breakpoints t
        with _ -> ())

let set_breakpoint t ~id ~source ~line ?(column = 0)
    ?(on_change = fun _ -> Lwt.return ()) () =
  let loc = { source; pos = (line, column); end_ = () } in
  let bp =
    {
      bp_id = id;
      bp_loc = loc;
      bp_resolved_loc = loc;
      bp_pc = None;
      bp_active = false;
      bp_version = 0;
      bp_on_change = on_change;
    }
  in
  Hashtbl.replace t.source_breakpoints id bp;
  _resolve_bp t bp;
  bp

let remove_breakpoint t bp =
  (match bp.bp_pc with
  | Some pc ->
      t.breakpoints <- t.breakpoints |> PcSet_.remove pc;
      Hashtbl.remove t.pc_to_bp pc;
      _schedule_sync_breakpoints t
  | _ -> ());
  Hashtbl.remove t.source_breakpoints bp.bp_id

let breakpoint_locations t source ~line ?column ?end_line ?end_column () =
  try
    let module_ = t.c.symbols |> Symbols.find_source_module source in
    let events =
      Code_module.find_events module_ ~line ?column ?end_line ?end_column ()
    in
    events
    |> List.map (fun event ->
           { source; pos = Util.Debug_event.line_column event; end_ = () })
  with Not_found -> []

let _summary_to_reason summary =
  match summary with
  | `Event -> Step
  | `Yield_stop 1 -> Pause
  | `Yield_stop _ | `Trap_barrier -> raise (Invalid_argument "summary")
  | `Breakpoint -> Breakpoint
  | `Exited -> Exited
  | `Uncaught_exc -> Uncaught_exc

let _wrap_run t f =
  if Controller.is_busy t.c || t.c.dead then Lwt.return ()
  else (
    Lwt.async (fun () ->
        Lwt.pause ();%lwt
        _sync_breakpoints t;%lwt
        t.scene <- None;
        t.set_state Running;
        let%lwt summary = f () in
        if not t.c.dead then t.scene <- Some (Scene.from_controller t.c);
        t.set_state (Stopped (_summary_to_reason summary));
        Lwt.return ());
    Lwt.return ())

let run t =
  _wrap_run t (fun () ->
      let on_yield _ =
        _sync_breakpoints t;%lwt
        Lwt.return
          (if t.interupt_flag then (
           t.interupt_flag <- false;
           `Stop 1)
          else `Continue)
      in
      let rec loop () =
        let%lwt summary, _, _ =
          Controller.execute ~yield_steps:t.opts.yield_steps ~on_yield t.c
            Int64.max_int
        in
        if summary = `Event then loop () else Lwt.return summary
      in
      loop ())

let step_in t =
  _wrap_run t (fun () ->
      let%lwt summary, _, _ = Controller.execute t.c _1 in
      Lwt.return summary)

let _go_out t (stack_pos, pc) =
  let on_yield _ =
    _sync_breakpoints t;%lwt
    Lwt.return
      (if t.interupt_flag then (
       t.interupt_flag <- false;
       `Stop 1)
      else `Continue)
  in
  let rec loop () =
    let%lwt summary, _, sp_pc =
      Controller.execute ~yield_steps:t.opts.yield_steps ~on_yield
        ~trap_barrier:stack_pos ~temporary_breakpoint:pc t.c Int64.max_int
    in
    let met =
      match sp_pc with
      | Some (stack_pos', pc') when stack_pos = stack_pos' && pc = pc' -> true
      | _ -> false
    in
    let met_bp = t.c.breakpoints |> PcSet_.mem pc in
    match summary with
    | `Exited | `Uncaught_exc | `Yield_stop _ -> Lwt.return summary
    | (`Trap_barrier | `Breakpoint) when met || met_bp -> Lwt.return `Event
    | _ -> loop ()
  in
  loop ()

let step_out t =
  if Controller.is_busy t.c || t.c.dead then Lwt.return ()
  else
    let frames = Scene.frames (t.c, t.c.time) in
    Lwt_stream.junk frames;%lwt
    let%lwt frame1 = Lwt_stream.get frames in
    match frame1 with
    | None -> failwith "Cannot step out"
    | Some frame ->
        _wrap_run t (fun () -> _go_out t (frame.stack_pos, frame.pc))

let next t =
  _wrap_run t (fun () ->
      let%lwt frame0 = Scene.top_frame (t.c, t.c.time) in
      let%lwt summary, _, _ = Controller.execute t.c _1 in
      let%lwt frame1 = Scene.top_frame (t.c, t.c.time) in
      let should_go_out (stack_pos1, e1) (stack_pos2, e2) =
        let is_entered =
          stack_pos2 - e2.ev_stacksize > stack_pos1 - e1.ev_stacksize
        in
        let is_tco =
          stack_pos2 - e2.ev_stacksize = stack_pos1 - e1.ev_stacksize
          && e2.ev_info = Event_function
        in
        is_entered || is_tco
      in
      match (frame0, frame1) with
      | ( Some ({ stack_pos = stack_pos1; event = Some e1; _ } as frame0),
          Some ({ stack_pos = stack_pos2; event = Some e2; _ } as frame1) )
        when should_go_out (stack_pos1, e1) (stack_pos2, e2) -> (
          let%lwt frame2 = Scene.next_frame (t.c, t.c.time) frame1 in
          match frame2 with
          | Some frame2 ->
              (* NOTE: Compiler may inline some primitive call. So step_in may enter a call at middle of inline code.
                 We need check that step_out is really returned into the same module of execution point before step_over.
                 This is a weak check, we could employ better mechanism to solve this problem.
                 One possible mechanism is: Find out next point in syntactic way and set breakpoint on there. *)
              let rec goto_same_module frame0 summary =
                let is_same_module frame1 frame2 =
                  match (frame1, frame2) with
                  | ( { pc = frag1, _; Frame.event = Some e1; _ },
                      { pc = frag2, _; Frame.event = Some e2; _ } ) ->
                      frag1 = frag2 && e1.ev_module = e2.ev_module
                  | _ -> false
                in
                match summary with
                | `Event -> (
                    match%lwt Scene.top_frame (t.c, t.c.time) with
                    | Some frame when is_same_module frame0 frame ->
                        Lwt.return summary
                    | _ ->
                        let%lwt summary, _, _ = Controller.execute t.c _1 in
                        goto_same_module frame0 summary)
                | _ -> Lwt.return summary
              in
              let%lwt summary = _go_out t (frame2.stack_pos, frame2.pc) in
              goto_same_module frame0 summary
          | None -> Lwt.return summary)
      | _ -> Lwt.return summary)
