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

  val get_field : conn -> remote_value -> int -> [`Remote_value of remote_value | `Double of float] Lwt.t

  val marshal_obj : conn -> remote_value -> 'a Lwt.t

  val get_closure_code : conn -> remote_value -> pc Lwt.t

  val set_fork_mode : conn -> fork_mode -> unit Lwt.t
end

include S
