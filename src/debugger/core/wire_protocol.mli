open Ground
open Debug_types

type pc = int * int

type execution_summary = [
  | `Event
  | `Breakpoint
  | `Exited
  | `Trap_barrier
  | `Uncaught_exc
  | `Debug_info of int Ident.Map.t * (Instruct.debug_event list * string list) list
  | `Code_loaded of int
  | `Code_unloaded of int
]

exception Float_field of float

type remote_value

val set_fork_mode : Lwt_conn.t -> [`Fork_child | `Fork_parent] -> unit Lwt.t

val set_event : Lwt_conn.t -> pc -> unit Lwt.t

val set_breakpoint : Lwt_conn.t -> pc -> unit Lwt.t

val reset_instr : Lwt_conn.t -> pc -> unit Lwt.t

val set_trap_barrier : Lwt_conn.t -> Sp.t -> unit Lwt.t

val checkpoint : Lwt_conn.t -> int Lwt.t

val go :
  Lwt_conn.t -> int -> (int * execution_summary * (Sp.t * pc) option) Lwt.t

val stop : Lwt_conn.t -> unit Lwt.t

val wait : Lwt_conn.t -> unit Lwt.t

val initial_frame : Lwt_conn.t -> (Sp.t * pc) Lwt.t

val get_frame : Lwt_conn.t -> (Sp.t * pc) Lwt.t

val set_frame : Lwt_conn.t -> Sp.t -> unit Lwt.t

val up_frame : Lwt_conn.t -> int -> (Sp.t * pc) option Lwt.t

val get_local : Lwt_conn.t -> int -> remote_value Lwt.t

val get_environment : Lwt_conn.t -> int -> remote_value Lwt.t

val get_global : Lwt_conn.t -> int -> remote_value Lwt.t

val get_accu : Lwt_conn.t -> remote_value Lwt.t

val get_header : Lwt_conn.t -> remote_value -> int Lwt.t

val get_field : Lwt_conn.t -> remote_value -> int -> remote_value Lwt.t

val marshal_obj : Lwt_conn.t -> remote_value -> 'a Lwt.t

val get_closure_code : Lwt_conn.t -> remote_value -> pc Lwt.t
