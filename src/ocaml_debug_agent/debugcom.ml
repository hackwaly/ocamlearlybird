open Debug_types

module type S = sig
  type conn

  val connect : remote_debugger_version -> Lwt_io.input_channel -> Lwt_io.output_channel -> conn Lwt.t

  val get_pid : conn -> int Lwt.t

  val set_event : conn -> pc -> unit Lwt.t

  val set_breakpoint : conn -> pc -> unit Lwt.t

  val reset_instr : conn -> pc -> unit Lwt.t

  val checkpoint : conn -> int Lwt.t

  val go : conn -> int -> report Lwt.t

  val stop : conn -> unit Lwt.t

  val wait : conn -> unit Lwt.t

  val initial_frame : conn -> (int * pc) Lwt.t

  val get_frame : conn -> (int * pc) Lwt.t

  val set_frame : conn -> int -> unit Lwt.t

  val up_frame : conn -> int -> (int * pc) option Lwt.t

  val set_trap_barrier : conn -> int -> unit Lwt.t

  val get_local : conn -> int -> remote_value Lwt.t

  val get_environment : conn -> int -> remote_value Lwt.t

  val get_global : conn -> int -> remote_value Lwt.t

  val get_accu : conn -> remote_value Lwt.t

  val get_header : conn -> remote_value -> int Lwt.t

  val get_field :
    conn ->
    remote_value ->
    int ->
    [ `Remote_value of remote_value | `Double of float ] Lwt.t

  val marshal_obj : conn -> remote_value -> 'a Lwt.t

  val get_closure_code : conn -> remote_value -> pc Lwt.t

  val set_fork_mode : conn -> fork_mode -> unit Lwt.t
end

type conn =
  | Conn : { impl : (module S with type conn = 'a); conn : 'a } -> conn

let connect version io_in io_out =
  let (module Impl) = match version with
    | OCaml_410 -> (module Debugcom_410 : S)
    | _ -> assert false
  in
  let%lwt conn = Impl.connect version io_in io_out  in
  Lwt.return (Conn { impl = (module Impl); conn })

let get_pid (Conn { impl = (module Impl); conn }) = Impl.get_pid conn

let set_event (Conn { impl = (module Impl); conn }) = Impl.set_event conn

let set_breakpoint (Conn { impl = (module Impl); conn }) =
  Impl.set_breakpoint conn

let reset_instr (Conn { impl = (module Impl); conn }) = Impl.reset_instr conn

let checkpoint (Conn { impl = (module Impl); conn }) = Impl.checkpoint conn

let go (Conn { impl = (module Impl); conn }) = Impl.go conn

let stop (Conn { impl = (module Impl); conn }) = Impl.stop conn

let wait (Conn { impl = (module Impl); conn }) = Impl.wait conn

let initial_frame (Conn { impl = (module Impl); conn }) =
  Impl.initial_frame conn

let get_frame (Conn { impl = (module Impl); conn }) = Impl.get_frame conn

let set_frame (Conn { impl = (module Impl); conn }) = Impl.set_frame conn

let up_frame (Conn { impl = (module Impl); conn }) = Impl.up_frame conn

let set_trap_barrier (Conn { impl = (module Impl); conn }) =
  Impl.set_trap_barrier conn

let get_local (Conn { impl = (module Impl); conn }) = Impl.get_local conn

let get_environment (Conn { impl = (module Impl); conn }) =
  Impl.get_environment conn

let get_global (Conn { impl = (module Impl); conn }) = Impl.get_global conn

let get_accu (Conn { impl = (module Impl); conn }) = Impl.get_accu conn

let get_header (Conn { impl = (module Impl); conn }) = Impl.get_header conn

let get_field (Conn { impl = (module Impl); conn }) = Impl.get_field conn

let marshal_obj (Conn { impl = (module Impl); conn }) = Impl.marshal_obj conn

let get_closure_code (Conn { impl = (module Impl); conn }) =
  Impl.get_closure_code conn

let set_fork_mode (Conn { impl = (module Impl); conn }) =
  Impl.set_fork_mode conn
