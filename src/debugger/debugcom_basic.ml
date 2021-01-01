type protocol_version = OCaml_400 | OCaml_410 [@@deriving show]

type conn =
  < io_in : Lwt_io.input_channel
  ; io_out : Lwt_io.output_channel
  ; protocol_version : protocol_version >

let read_pc conn =
  let%lwt frag = Lwt_io.BE.read_int conn#io_in in
  let%lwt pos = Lwt_io.BE.read_int conn#io_in in
  let pc = { Pc.frag; pos } in
  Lwt.return pc

let write_pc conn pc =
  Lwt_io.BE.write_int conn#io_out pc.Pc.frag;%lwt
  Lwt_io.BE.write_int conn#io_out pc.pos

let get_pid conn =
  let%lwt neg_one = Lwt_io.BE.read_int conn#io_in in
  [%lwt assert (neg_one = -1)];%lwt
  let%lwt pid = Lwt_io.BE.read_int conn#io_in in
  Lwt.return pid

let set_event conn pc =
  Lwt_io.write_char conn#io_out 'e';%lwt
  write_pc conn pc

let set_breakpoint conn pc =
  Lwt_io.write_char conn#io_out 'B';%lwt
  write_pc conn pc

let reset_instr conn pc =
  Lwt_io.write_char conn#io_out 'i';%lwt
  write_pc conn pc

exception Checkpoint_failure

let checkpoint conn =
  assert (not Sys.win32);
  let%lwt pid = Lwt_io.BE.read_int conn#io_in in
  if pid = -1 then Lwt.fail Checkpoint_failure else Lwt.return pid

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
  rep_program_pointer : Pc.t;
}
[@@deriving show]

let go conn n =
  Lwt_io.write_char conn#io_out 'g';%lwt
  Lwt_io.BE.write_int conn#io_out n;%lwt
  let%lwt summary =
    match%lwt Lwt_io.read_char conn#io_in with
    | 'e' -> Lwt.return Event
    | 'b' -> Lwt.return Breakpoint
    | 'x' -> Lwt.return Exited
    | 's' -> Lwt.return Trap
    | 'u' -> Lwt.return Uncaught_exc
    | 'D' ->
        let%lwt eventlists = Lwt_io.read_value conn#io_in in
        Lwt.return (Code_debug_info { eventlists })
    | 'L' ->
        let%lwt frag = Lwt_io.BE.read_int conn#io_in in
        Lwt.return (Code_loaded frag)
    | 'U' ->
        let%lwt frag = Lwt_io.BE.read_int conn#io_in in
        Lwt.return (Code_unloaded frag)
    | _ -> [%lwt assert false]
  in
  let%lwt event_counter = Lwt_io.BE.read_int conn#io_in in
  let%lwt stack_pos = Lwt_io.BE.read_int conn#io_in in
  let%lwt pc = read_pc conn in
  let report =
    {
      rep_type = summary;
      rep_event_count = Int64.of_int event_counter;
      rep_stack_pointer = stack_pos;
      rep_program_pointer = pc;
    }
  in
  Lwt.return report

let stop conn = Lwt_io.write_char conn#io_out 's'

let wait conn = Lwt_io.write_char conn#io_out 'w'

let initial_frame conn =
  Lwt_io.write_char conn#io_out '0';%lwt
  let%lwt stack_pos = Lwt_io.BE.read_int conn#io_in in
  let%lwt pc = read_pc conn in
  Lwt.return (stack_pos, pc)

let get_frame conn =
  Lwt_io.write_char conn#io_out 'f';%lwt
  let%lwt stack_pos = Lwt_io.BE.read_int conn#io_in in
  let%lwt pc = read_pc conn in
  Lwt.return (stack_pos, pc)

let set_frame conn stack_pos =
  Lwt_io.write_char conn#io_out 'S';%lwt
  Lwt_io.BE.write_int conn#io_out stack_pos

let up_frame conn stacksize =
  Lwt_io.write_char conn#io_out 'U';%lwt
  Lwt_io.BE.write_int conn#io_out stacksize;%lwt
  let%lwt stack_pos = Lwt_io.BE.read_int conn#io_in in
  let%lwt res =
    if stack_pos = -1 then Lwt.return None
    else
      let%lwt pc = read_pc conn in
      Lwt.return (Some (stack_pos, pc))
  in
  Lwt.return res

let set_trap_barrier conn pos =
  Lwt_io.write_char conn#io_out 'b';%lwt
  Lwt_io.BE.write_int conn#io_out pos

type remote_value = nativeint [@@deriving show]

let get_local conn index =
  Lwt_io.write_char conn#io_out 'L';%lwt
  Lwt_io.BE.write_int conn#io_out index;%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn#io_in in
  Lwt.return rv

let get_environment conn index =
  Lwt_io.write_char conn#io_out 'E';%lwt
  Lwt_io.BE.write_int conn#io_out index;%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn#io_in in
  Lwt.return rv

let get_global conn index =
  Lwt_io.write_char conn#io_out 'G';%lwt
  Lwt_io.BE.write_int conn#io_out index;%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn#io_in in
  Lwt.return rv

let get_accu conn =
  Lwt_io.write_char conn#io_out 'A';%lwt
  let%lwt rv = Lwt_util.read_nativeint_be conn#io_in in
  Lwt.return rv

let get_header conn rv =
  Lwt_io.write_char conn#io_out 'H';%lwt
  Lwt_util.write_nativeint_be conn#io_out rv;%lwt
  let%lwt hdr = Lwt_io.BE.read_int conn#io_in in
  Lwt.return hdr

exception Double_field of float

let get_field conn rv index =
  Lwt_io.write_char conn#io_out 'F';%lwt
  Lwt_util.write_nativeint_be conn#io_out rv;%lwt
  Lwt_io.BE.write_int conn#io_out index;%lwt
  let%lwt res =
    match%lwt Lwt_io.read_char conn#io_in with
    | '\000' ->
        let%lwt rv = Lwt_util.read_nativeint_be conn#io_in in
        Lwt.return rv
    | '\001' ->
        let%lwt v = Lwt_io.read_float64 conn#io_in in
        Lwt.fail (Double_field v)
    | _ -> [%lwt assert false]
  in
  Lwt.return res

let marshal_obj conn rv =
  Lwt_io.write_char conn#io_out 'M';%lwt
  Lwt_util.write_nativeint_be conn#io_out rv;%lwt
  let%lwt v = Lwt_io.read_value conn#io_in in
  Lwt.return v

let get_closure_code conn rv =
  Lwt_io.write_char conn#io_out 'C';%lwt
  Lwt_util.write_nativeint_be conn#io_out rv;%lwt
  let%lwt pc = read_pc conn in
  Lwt.return pc

type fork_mode = Fork_child | Fork_parent [@@deriving show]

let set_fork_mode conn mode =
  Lwt_io.write_char conn#io_out 'K';%lwt
  Lwt_io.BE.write_int conn#io_out
    (match mode with Fork_child -> 0 | Fork_parent -> 1)