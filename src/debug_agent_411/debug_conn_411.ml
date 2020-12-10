[@@@warning "-27"]
module Symbols = Symbols_411

let src = Logs.Src.create "earlybird.Debug_conn_411"
module Log = (val Logs_lwt.src_log src : Logs_lwt.LOG)

type follow_fork_mode =
  | Fork_parent
  | Fork_child
[@@deriving show]

type execution_summary =
  | Event
  | Breakpoint
  | Exited
  | Trap_barrier
  | Uncaught_exc
  | Debug_info of Instruct.debug_event list array
  | Code_loaded of int
  | Code_unloaded of int

type pc = Symbols.pc = {
  frag : int;
  pos : int;
}

type report = {
  rep_type : execution_summary;
  rep_event_count : int;
  rep_stack_pointer : int;
  rep_program_pointer : pc;
}

type t = {
  in_chan : Lwt_io.input_channel;
  out_chan : Lwt_io.output_channel;
  mutex : Lwt_mutex.t option;
}

let create in_chan out_chan =
  { in_chan; out_chan; mutex = Some (Lwt_mutex.create ()) }

let guard conn fn =
  match conn.mutex with
  | Some mutex -> (
      Lwt_mutex.lock mutex;%lwt
      (
        (* try%lwt *)
          fn { conn with mutex = None }
        (* with
          | End_of_file
          | Lwt_io.Channel_closed _ -> Lwt.fail Exit *)
      )[%finally Lwt_mutex.unlock mutex; Lwt.return_unit]
    )
  | None -> fn conn

let initial conn =
  guard conn (fun conn ->
    let%lwt _ = Lwt_io.BE.read_int conn.in_chan in
    let%lwt pid = Lwt_io.BE.read_int conn.in_chan in
    Lwt.return pid
  )

let stop conn =
  guard conn (fun conn ->
    try%lwt
      Lwt_io.write_char conn.out_chan 's';%lwt
      Lwt_io.flush conn.out_chan
    with Sys_error _ | End_of_file -> Lwt.return_unit
  )

let set_follow_fork_mode conn mode =
  guard conn (fun conn ->
    let v = match mode with Fork_parent -> 1 | Fork_child -> 0 in
    Lwt_io.write_char conn.out_chan 'K';%lwt
    Lwt_io.BE.write_int conn.out_chan v
  )

let set_event conn {frag; pos} =
  guard conn (fun conn ->
    Lwt_io.write_char conn.out_chan 'e';%lwt
    Lwt_io.BE.write_int conn.out_chan frag;%lwt
    Lwt_io.BE.write_int conn.out_chan pos
  )

let set_breakpoint conn {frag; pos} =
  guard conn (fun conn ->
    Lwt_io.write_char conn.out_chan 'B';%lwt
    Lwt_io.BE.write_int conn.out_chan frag;%lwt
    Lwt_io.BE.write_int conn.out_chan pos
  )

let reset_instruction conn {frag; pos} =
  guard conn (fun conn ->
    Lwt_io.write_char conn.out_chan 'i';%lwt
    Lwt_io.BE.write_int conn.out_chan frag;%lwt
    Lwt_io.BE.write_int conn.out_chan pos
  )

let set_trap_barrier conn pos =
  guard conn (fun conn ->
    Lwt_io.write_char conn.out_chan 'b';%lwt
    Lwt_io.BE.write_int conn.out_chan pos
  )

let exec_with_trap_barrier conn trap_barrier func =
  guard conn (fun conn ->
    set_trap_barrier conn trap_barrier;%lwt
    (func conn)[%finally set_trap_barrier conn 0]
  )

let exec_with_temporary_breakpoint conn pos func =
  guard conn (fun conn ->
    set_breakpoint conn pos;%lwt
    (func conn)[%finally reset_instruction conn pos]
  )

let value_size = if 1 lsl 31 = 0 then 4 else 8

let input_remote_value conn =
  guard conn (fun conn ->
    let buf = Bytes.create value_size in
    Lwt_io.read_into_exactly conn.in_chan buf 0 value_size;%lwt
    Lwt.return (Bytes.to_string buf)
  )

let output_remote_value conn value =
  guard conn (fun conn ->
    Lwt_io.write_from_string_exactly conn.out_chan value 0 value_size
  )

let go conn n =
  guard conn (fun conn ->
    Lwt_io.write_char conn.out_chan 'g';%lwt
    Lwt_io.BE.write_int conn.out_chan n;%lwt
    let%lwt char = Lwt_io.read_char conn.in_chan in
    let%lwt summary = match char with
      | 'e' -> Lwt.return Event
      | 'b' -> Lwt.return Breakpoint
      | 'x' -> Lwt.return Exited
      | 's' -> Lwt.return Trap_barrier
      | 'u' -> Lwt.return Uncaught_exc
      | 'D' -> (
        let%lwt debug_info = Lwt_io.read_value conn.in_chan in
        Lwt.return (Debug_info (debug_info : Instruct.debug_event list array))
      )
      | 'L' -> (
        let%lwt n = Lwt_io.BE.read_int conn.in_chan in
        Lwt.return (Code_loaded n)
      )
      | 'U' -> (
        let%lwt n = Lwt_io.BE.read_int conn.in_chan in
        Lwt.return (Code_unloaded n)
      )
      |  _  -> assert false in
    let%lwt event_counter = Lwt_io.BE.read_int conn.in_chan in
    let%lwt stack_pos = Lwt_io.BE.read_int conn.in_chan in
    let%lwt frag = Lwt_io.BE.read_int conn.in_chan in
    let%lwt pos = Lwt_io.BE.read_int conn.in_chan in
    let report = {
      rep_type = summary;
      rep_event_count = event_counter;
      rep_stack_pointer = stack_pos;
      rep_program_pointer = {frag; pos};
    } in
    Lwt.return report
  )

let get_frame conn =
  guard conn (fun conn ->
    Lwt_io.write_char conn.out_chan 'f';%lwt
    let%lwt stack_pos = Lwt_io.BE.read_int conn.in_chan in
    let%lwt pc = Lwt_io.BE.read_int conn.in_chan in
    Lwt.return (stack_pos, pc)
  )

let set_frame conn stack_pos =
  guard conn (fun conn ->
    Lwt_io.write_char conn.out_chan 'S';%lwt
    Lwt_io.BE.write_int conn.out_chan stack_pos
  )

let initial_frame conn  =
  guard conn (fun conn ->
    Lwt_io.write_char conn.out_chan '0';%lwt
    let%lwt stack_pos = Lwt_io.BE.read_int conn.in_chan in
    let%lwt pc = Lwt_io.BE.read_int conn.in_chan in
    Lwt.return (stack_pos, pc)
  )

let up_frame conn stack_size =
  guard conn (fun conn ->
    Lwt_io.write_char conn.out_chan 'U';%lwt
    Lwt_io.BE.write_int conn.out_chan stack_size;%lwt
    let%lwt stack_pos = Lwt_io.BE.read_int conn.in_chan in
    (* See https://github.com/hackwaly/ocamlearlybird/issues/14 *)
    if stack_pos < 0 then Lwt.return None
    else (
      let%lwt pc = Lwt_io.BE.read_int conn.in_chan in
      Lwt.return (Some (stack_pos, pc))
    )
  )

module Remote_value = struct
  type t =
    | Remote of string
    | Local of Obj.t

  let same rv1 rv2 =
    match rv1, rv2  with
    | Local obj1, Local obj2 -> obj1 == obj2
    | Remote rv1, Remote rv2 -> rv1 = rv2
    | _ -> false

  let repr x = Local (Obj.repr x)

  let obj conn rv =
    match rv with
    | Local obj -> Lwt.return (Obj.obj obj)
    | Remote rv ->
      guard conn (fun conn ->
        Lwt_io.write_char conn.out_chan 'M';%lwt
        output_remote_value conn rv;%lwt
        Lwt_io.read_value conn.in_chan
      )

  let is_block rv =
    match rv with
    | Local obj -> Obj.is_block obj
    | Remote rv -> Obj.is_block (Array.unsafe_get (Obj.magic rv : Obj.t array) 0)

  let tag conn rv =
    if not (is_block rv) then Lwt.return Obj.int_tag
    else
      match rv with
      | Local obj -> Lwt.return (Obj.tag obj)
      | Remote rv ->
        guard conn (fun conn ->
          Lwt_io.write_char conn.out_chan 'H';%lwt
          output_remote_value conn rv;%lwt
          let%lwt header = Lwt_io.BE.read_int conn.in_chan in
          Lwt.return (header land 0xFF)
        )

  let size conn rv =
    match rv with
    | Local obj -> Lwt.return (Obj.size obj)
    | Remote rv ->
      guard conn (fun conn ->
        Lwt_io.write_char conn.out_chan 'H';%lwt
        output_remote_value conn rv;%lwt
        let%lwt header = Lwt_io.BE.read_int conn.in_chan in
        Lwt.return (
          if header land 0xFF = Obj.double_array_tag && Sys.word_size = 32
          then header lsr 11
          else header lsr 10
        )
      )

  let field conn rv idx =
    match rv with
    | Local obj -> Lwt.return (Local (Obj.field obj idx))
    | Remote rv ->
      guard conn (fun conn ->
        Lwt_io.write_char conn.out_chan 'F';%lwt
        output_remote_value conn rv;%lwt
        Lwt_io.BE.write_int conn.out_chan idx;%lwt
        match%lwt Lwt_io.read_char conn.in_chan with
        | '\000' ->
          let%lwt value = input_remote_value conn in
          Lwt.return (Remote value)
        | '\001' ->
          (* Not big-endian here *)
          let%lwt value = Lwt_io.read_float64 conn.in_chan in
          Lwt.return (Local (Obj.repr value))
        | _ -> assert false
      )

  let local conn pos =
    guard conn (fun conn ->
      Lwt_io.write_char conn.out_chan 'L';%lwt
      Lwt_io.BE.write_int conn.out_chan pos;%lwt
      let%lwt rv = input_remote_value conn in
      Lwt.return (Remote rv)
    )

  let from_environment conn pos =
    guard conn (fun conn ->
      Lwt_io.write_char conn.out_chan 'E';%lwt
      Lwt_io.BE.write_int conn.out_chan pos;%lwt
      let%lwt rv = input_remote_value conn in
      Lwt.return (Remote rv)
    )

  let global conn pos =
    guard conn (fun conn ->
      Lwt_io.write_char conn.out_chan 'G';%lwt
      Lwt_io.BE.write_int conn.out_chan pos;%lwt
      let%lwt rv = input_remote_value conn in
      Lwt.return (Remote rv)
    )

  let accu conn pos =
    guard conn (fun conn ->
      Lwt_io.write_char conn.out_chan 'A';%lwt
      let%lwt rv = input_remote_value conn in
      Lwt.return (Remote rv)
    )

  let closure_code conn rv =
    match rv with
    | Local _ -> assert false
    | Remote rv ->
      guard conn (fun conn ->
        Lwt_io.write_char conn.out_chan 'C';%lwt
        output_remote_value conn rv;%lwt
        Lwt_io.BE.read_int conn.in_chan
      )

  let pointer rv =
    match rv with
    | Local _ -> ""
    | Remote rv ->
      let bytes = ref [] in
      String.iter (fun c -> bytes := c :: !bytes) rv;
      let obytes = if Sys.big_endian then List.rev !bytes else !bytes in
      let to_hex c = Printf.sprintf "%02x" (Char.code c) in
      String.concat "" (List.map to_hex obytes)

end