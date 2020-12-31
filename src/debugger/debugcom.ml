type remote_debugger_version =
  | OCaml_400
  | OCaml_410

type pc = Pc.t = {
  frag : int;
  pos : int;
}
[@@deriving show]

type fork_mode = Fork_child | Fork_parent [@@deriving show]

type debug_info = { eventlists : Instruct.debug_event list array [@opaque] }
[@@deriving show]

type execution_summary =
  | Event
  | Breakpoint
  | Exited
  | Trap
  | Uncaught_exc
  | Code_debug_info of debug_info
  | Code_loaded of int
  | Code_unloaded of int
[@@deriving show]

type report = {
  rep_type : execution_summary;
  rep_event_count : int64;
  rep_stack_pointer : int;
  rep_program_pointer : pc;
}
[@@deriving show]

type remote_value = nativeint [@@deriving show]

exception Checkpoint_failure

type conn = {
  version : remote_debugger_version;
  io_in : Lwt_io.input_channel;
  io_out : Lwt_io.output_channel;
}

let connect version io_in io_out =
  [%lwt assert (version = OCaml_410)];%lwt
  Lwt.return { version; io_in; io_out }

let get_pid conn =
  Log.debug (fun m -> m "get_pid");%lwt
  let%lwt neg_one = Lwt_io.BE.read_int conn.io_in in
  [%lwt assert (neg_one = -1)];%lwt
  let%lwt pid = Lwt_io.BE.read_int conn.io_in in
  Log.debug (fun m -> m "get_pid return %d" pid);%lwt
  Lwt.return pid

let set_event conn pc =
  Log.debug (fun m -> m "set_event pc:%s" (show_pc pc));%lwt
  Lwt_io.write_char conn.io_out 'e';%lwt
  Lwt_io.BE.write_int conn.io_out pc.frag;%lwt
  Lwt_io.BE.write_int conn.io_out pc.pos;%lwt
  Log.debug (fun m -> m "set_event return")

let set_breakpoint conn pc =
  Log.debug (fun m -> m "set_breakpoint pc:%s" (show_pc pc));%lwt
  Lwt_io.write_char conn.io_out 'B';%lwt
  Lwt_io.BE.write_int conn.io_out pc.frag;%lwt
  Lwt_io.BE.write_int conn.io_out pc.pos;%lwt
  Log.debug (fun m -> m "set_breakpoint return")

let reset_instr conn pc =
  Log.debug (fun m -> m "reset_instr pc:%s" (show_pc pc));%lwt
  Lwt_io.write_char conn.io_out 'i';%lwt
  Lwt_io.BE.write_int conn.io_out pc.frag;%lwt
  Lwt_io.BE.write_int conn.io_out pc.pos;%lwt
  Log.debug (fun m -> m "reset_instr return")

let checkpoint conn =
  Log.debug (fun m -> m "checkpoint");%lwt
  assert (not Sys.win32);
  let%lwt pid = Lwt_io.BE.read_int conn.io_in in
  if pid = -1 then Lwt.fail Checkpoint_failure
  else (
    Log.debug (fun m -> m "checkpoint return %d" pid);%lwt
    Lwt.return pid )

let go conn n =
  Log.debug (fun m -> m "go n:%d" n);%lwt
  Lwt_io.write_char conn.io_out 'g';%lwt
  Lwt_io.BE.write_int conn.io_out n;%lwt
  let%lwt summary =
    match%lwt Lwt_io.read_char conn.io_in with
    | 'e' -> Lwt.return Event
    | 'b' -> Lwt.return Breakpoint
    | 'x' -> Lwt.return Exited
    | 's' -> Lwt.return Trap
    | 'u' -> Lwt.return Uncaught_exc
    | 'D' ->
        let%lwt eventlists = Lwt_io.read_value conn.io_in in
        Lwt.return (Code_debug_info { eventlists })
    | 'L' ->
        let%lwt frag = Lwt_io.BE.read_int conn.io_in in
        Lwt.return (Code_loaded frag)
    | 'U' ->
        let%lwt frag = Lwt_io.BE.read_int conn.io_in in
        Lwt.return (Code_unloaded frag)
    | _ -> [%lwt assert false]
  in
  let%lwt event_counter = Lwt_io.BE.read_int conn.io_in in
  let%lwt stack_pos = Lwt_io.BE.read_int conn.io_in in
  let%lwt frag = Lwt_io.BE.read_int conn.io_in in
  let%lwt pos = Lwt_io.BE.read_int conn.io_in in
  let report =
    {
      rep_type = summary;
      rep_event_count = Int64.of_int event_counter;
      rep_stack_pointer = stack_pos;
      rep_program_pointer = { frag; pos };
    }
  in
  Log.debug (fun m -> m "go return %s" (show_report report));%lwt
  Lwt.return report

let stop conn =
  Log.debug (fun m -> m "stop");%lwt
  Lwt_io.write_char conn.io_out 's';%lwt
  Log.debug (fun m -> m "stop return")

let wait conn =
  Log.debug (fun m -> m "wait");%lwt
  Lwt_io.write_char conn.io_out 'w';%lwt
  Log.debug (fun m -> m "wait return")

let initial_frame conn =
  Log.debug (fun m -> m "initial_frame");%lwt
  Lwt_io.write_char conn.io_out '0';%lwt
  let%lwt stack_pos = Lwt_io.BE.read_int conn.io_in in
  let%lwt frag = Lwt_io.BE.read_int conn.io_in in
  let%lwt pos = Lwt_io.BE.read_int conn.io_in in
  Log.debug (fun m ->
      m "initial_frame return %s" ([%show: int * pc] (stack_pos, { frag; pos })));%lwt
  Lwt.return (stack_pos, { frag; pos })

let get_frame conn =
  Log.debug (fun m -> m "get_frame");%lwt
  Lwt_io.write_char conn.io_out 'f';%lwt
  let%lwt stack_pos = Lwt_io.BE.read_int conn.io_in in
  let%lwt frag = Lwt_io.BE.read_int conn.io_in in
  let%lwt pos = Lwt_io.BE.read_int conn.io_in in
  Log.debug (fun m ->
      m "get_frame return %s" ([%show: int * pc] (stack_pos, { frag; pos })));%lwt
  Lwt.return (stack_pos, { frag; pos })

let set_frame conn stack_pos =
  Log.debug (fun m -> m "set_frame stack_pos:%d" stack_pos);%lwt
  Lwt_io.write_char conn.io_out 'S';%lwt
  Lwt_io.BE.write_int conn.io_out stack_pos;%lwt
  Log.debug (fun m -> m "set_frame return")

let up_frame conn stacksize =
  Log.debug (fun m -> m "up_frame stacksize:%d" stacksize);%lwt
  Lwt_io.write_char conn.io_out 'U';%lwt
  Lwt_io.BE.write_int conn.io_out stacksize;%lwt
  let%lwt stack_pos = Lwt_io.BE.read_int conn.io_in in
  let%lwt res =
    if stack_pos = -1 then Lwt.return None
    else
      let%lwt frag = Lwt_io.BE.read_int conn.io_in in
      let%lwt pos = Lwt_io.BE.read_int conn.io_in in
      Lwt.return (Some (stack_pos, { frag; pos }))
  in
  Log.debug (fun m -> m "set_frame return %s" ([%show: (int * pc) option] res));%lwt
  Lwt.return res

let set_trap_barrier conn pos =
  Log.debug (fun m -> m "set_trap_barrier pos:%d" pos);%lwt
  Lwt_io.write_char conn.io_out 'b';%lwt
  Lwt_io.BE.write_int conn.io_out pos;%lwt
  Log.debug (fun m -> m "set_frame return")

let get_local conn index =
  Log.debug (fun m -> m "get_local index:%d" index);%lwt
  Lwt_io.write_char conn.io_out 'L';%lwt
  Lwt_io.BE.write_int conn.io_out index;%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn.io_in in
  Log.debug (fun m -> m "get_local return %s" (show_remote_value rv));%lwt
  Lwt.return rv

let get_environment conn index =
  Log.debug (fun m -> m "get_environment index:%d" index);%lwt
  Lwt_io.write_char conn.io_out 'E';%lwt
  Lwt_io.BE.write_int conn.io_out index;%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn.io_in in
  Log.debug (fun m -> m "get_environment return %s" (show_remote_value rv));%lwt
  Lwt.return rv

let get_global conn index =
  Log.debug (fun m -> m "get_global index:%d" index);%lwt
  Lwt_io.write_char conn.io_out 'G';%lwt
  Lwt_io.BE.write_int conn.io_out index;%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn.io_in in
  Log.debug (fun m -> m "get_global return %s" (show_remote_value rv));%lwt
  Lwt.return rv

let get_accu conn =
  Log.debug (fun m -> m "get_accu");%lwt
  Lwt_io.write_char conn.io_out 'A';%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn.io_in in
  Log.debug (fun m -> m "get_accu return %s" (show_remote_value rv));%lwt
  Lwt.return rv

let get_header conn rv =
  Log.debug (fun m -> m "get_header rv:%s" (show_remote_value rv));%lwt
  Lwt_io.write_char conn.io_out 'H';%lwt
  Lwt_util.write_nativeint_be conn.io_out rv;%lwt
  let%lwt hdr = Lwt_io.BE.read_int conn.io_in in
  Log.debug (fun m -> m "get_header return %d" hdr);%lwt
  Lwt.return hdr

let get_field conn rv index =
  Log.debug (fun m -> m "get_field index:%d" index);%lwt
  Lwt_io.write_char conn.io_out 'F';%lwt
  Lwt_util.write_nativeint_be conn.io_out rv;%lwt
  Lwt_io.BE.write_int conn.io_out index;%lwt
  let%lwt res =
    match%lwt Lwt_io.read_char conn.io_in with
    | '\000' ->
        let%lwt rv = Lwt_util.read_nativeint_be conn.io_in in
        Lwt.return (`Remote_value rv)
    | '\001' ->
        let%lwt v = Lwt_io.read_float64 conn.io_in in
        Lwt.return (`Double v)
    | _ -> [%lwt assert false]
  in
  Log.debug (fun m ->
      m "get_field return %s"
        ([%show: [ `Remote_value of remote_value | `Double of float ]] res));%lwt
  Lwt.return res

let marshal_obj conn rv =
  Log.debug (fun m -> m "marshal_obj rv:%s" (show_remote_value rv));%lwt
  Lwt_io.write_char conn.io_out 'M';%lwt
  Lwt_util.write_nativeint_be conn.io_out rv;%lwt
  let%lwt v = Lwt_io.read_value conn.io_in in
  Log.debug (fun m -> m "marshal_obj return obj");%lwt
  Lwt.return v

let get_closure_code conn rv =
  Log.debug (fun m -> m "get_closure_code rv:%s" (show_remote_value rv));%lwt
  Lwt_io.write_char conn.io_out 'C';%lwt
  Lwt_util.write_nativeint_be conn.io_out rv;%lwt
  let%lwt frag = Lwt_io.BE.read_int conn.io_in in
  let%lwt pos = Lwt_io.BE.read_int conn.io_in in
  Log.debug (fun m -> m "get_closure_code return %s" (show_pc { frag; pos }));%lwt
  Lwt.return { frag; pos }

let set_fork_mode conn mode =
  Log.debug (fun m -> m "set_fork_mode mode:%s" (show_fork_mode mode));%lwt
  Lwt_io.write_char conn.io_out 'K';%lwt
  Lwt_io.BE.write_int conn.io_out
    (match mode with Fork_child -> 0 | Fork_parent -> 1);%lwt
  Log.debug (fun m -> m "set_fork_mode return")
