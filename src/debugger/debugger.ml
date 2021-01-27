open Ground
open Path_glob
open Int64ops
open Instruct
module PcSet_ = Set.Make (Ordered_type.Make_tuple2 (Int) (Int))

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
  only_debug_glob : Glob.globber option; [@default None]
}
[@@deriving make]

type reason = Entry | Step | Breakpoint | Pause | Exited | Uncaught_exc

type state = Running | Stopped of reason

type breakpoint = {
  bp_id : int;
  bp_loc : source_position;
  bp_active : bool Lwt_react.S.t;
  bp_set_active : bool -> unit;
}

type t = {
  opts : options;
  c : Controller.t;
  mutable interupt_flag : bool;
  state : state Lwt_react.S.t;
  set_state : state -> unit;
  last_sync_symver : int;
  mutable next_bp_id : int;
  mutable breakpoints : breakpoint list;
  mutable pending_breakpoints : breakpoint list;
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
  let { only_debug_glob; debug_sock; symbols_file; _ } = opts in
  let source_resolver = Util.Source_resolver.make ?only_debug_glob () in
  let%lwt c = Controller.root ~source_resolver debug_sock symbols_file in
  Controller.set_follow_fork_mode c opts.follow_fork_mode;%lwt
  let state, set_state = Lwt_react.S.create (Stopped Entry) in
  Lwt.return
    {
      opts;
      c;
      interupt_flag = false;
      state;
      set_state;
      last_sync_symver = -1;
      next_bp_id = 0;
      breakpoints = [];
      pending_breakpoints = [];
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
          let* event = frame.Frame.event in
          return event.ev_defname
        in
        {
          index = frame.Frame.index;
          name;
          loc = frame.loc;
          _frame = frame;
          scopes =
            ( match frame.event with
            | Some _ ->
                [
                  ("Stack", Inspect.scope scene frame `Stack);
                  ("Heap", Inspect.scope scene frame `Heap);
                  ("Global", Inspect.scope scene frame `Global);
                ]
            | None -> [] );
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

let _next_bp_id t =
  let id = t.next_bp_id in
  t.next_bp_id <- id + 1;
  id

let set_breakpoint t source line column =
  let bp_active, bp_set_active = Lwt_react.S.create false in
  let bp =
    {
      bp_id = _next_bp_id t;
      bp_loc = { source; pos = (line, column); end_ = () };
      bp_active;
      bp_set_active;
    }
  in
  t.pending_breakpoints <- bp :: t.pending_breakpoints;
  Lwt.return bp

let _sync_breakpoints t =
  ignore t;
  Lwt.return ()

let _summary_to_reason summary =
  match summary with
  | `Event -> Step
  | `Yield_stop 1 -> Pause
  | `Yield_stop _ | `Trap_barrier -> raise (Invalid_argument "summary")
  | `Breakpoint -> Breakpoint
  | `Exited -> Exited
  | `Uncaught_exc -> Uncaught_exc

let _wrap_run t f =
  assert ((not (Controller.is_busy t.c)) && not t.c.dead);
  Lwt.async (fun () ->
      _sync_breakpoints t;%lwt
      t.scene <- None;
      t.set_state Running;
      let%lwt summary = f () in
      if not t.c.dead then t.scene <- Some (Scene.from_controller t.c);
      t.set_state (Stopped (_summary_to_reason summary));
      Lwt.return ());
  Lwt.return ()

let run t =
  _wrap_run t (fun () ->
      let on_yield _ =
        _sync_breakpoints t;%lwt
        Lwt.return
          ( if t.interupt_flag then (
            t.interupt_flag <- false;
            `Stop 1 )
          else `Continue )
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
      ( if t.interupt_flag then (
        t.interupt_flag <- false;
        `Stop 1 )
      else `Continue )
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
  assert ((not (Controller.is_busy t.c)) && not t.c.dead);
  let frames = Scene.frames (t.c, t.c.time) in
  Lwt_stream.junk frames;%lwt
  let%lwt frame1 = Lwt_stream.get frames in
  match frame1 with
  | None -> failwith "Cannot step out"
  | Some frame -> _wrap_run t (fun () -> _go_out t (frame.stack_pos, frame.pc))

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
      | ( Some { stack_pos = stack_pos1; event = Some e1; _ },
          Some ({ stack_pos = stack_pos2; event = Some e2; _ } as frame1) )
        when should_go_out (stack_pos1, e1) (stack_pos2, e2) -> (
          let%lwt frame2 = Scene.next_frame (t.c, t.c.time) frame1 in
          match frame2 with
          | Some frame2 -> _go_out t (frame2.stack_pos, frame2.pc)
          | None -> Lwt.return summary )
      | _ -> Lwt.return summary)
