type pc = { frag : int; pos : int } [@@deriving show]

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

type checkpoint_report = Checkpoint_done of int | Checkpoint_failed
[@@deriving show]

type remote_value = nativeint [@@deriving show]

type get_field_result = Remote_value of remote_value | Double of float
[@@deriving show]

module type BASIC = sig
  type conn

  val get_pid : conn -> int Lwt.t

  val set_event : conn -> pc -> unit Lwt.t

  val set_breakpoint : conn -> pc -> unit Lwt.t

  val reset_instr : conn -> pc -> unit Lwt.t

  val checkpoint : conn -> checkpoint_report Lwt.t

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

  val get_field : conn -> remote_value -> int -> get_field_result Lwt.t

  val marshal_obj : conn -> remote_value -> 'a Lwt.t

  val get_closure_code : conn -> remote_value -> pc Lwt.t

  val set_fork_mode : conn -> fork_mode -> unit Lwt.t
end

type conn =
  | Conn : { basic : (module BASIC with type conn = 'a); conn : 'a } -> conn

let create_conn (type conn) (module Basic : BASIC with type conn = conn) conn  =
  Conn { basic = (module Basic); conn }

[@@@ocamlformat "disable"]

let get_pid (Conn {basic = (module Basic); conn}) = Basic.get_pid conn

let set_event (Conn {basic = (module Basic); conn}) = Basic.set_event conn

let set_breakpoint (Conn {basic = (module Basic); conn}) = Basic.set_breakpoint conn

let reset_instr (Conn {basic = (module Basic); conn}) = Basic.reset_instr conn

let checkpoint (Conn {basic = (module Basic); conn}) = Basic.checkpoint conn

let go (Conn {basic = (module Basic); conn}) = Basic.go conn

let stop (Conn {basic = (module Basic); conn}) = Basic.stop conn

let wait (Conn {basic = (module Basic); conn}) = Basic.wait conn

let initial_frame (Conn {basic = (module Basic); conn}) = Basic.initial_frame conn

let get_frame (Conn {basic = (module Basic); conn}) = Basic.get_frame conn

let set_frame (Conn {basic = (module Basic); conn}) = Basic.set_frame conn

let up_frame (Conn {basic = (module Basic); conn}) = Basic.up_frame conn

let set_trap_barrier (Conn {basic = (module Basic); conn}) = Basic.set_trap_barrier conn

let get_local (Conn {basic = (module Basic); conn}) = Basic.get_local conn

let get_environment (Conn {basic = (module Basic); conn}) = Basic.get_environment conn

let get_global (Conn {basic = (module Basic); conn}) = Basic.get_global conn

let get_accu (Conn {basic = (module Basic); conn}) = Basic.get_accu conn

let get_header (Conn {basic = (module Basic); conn}) = Basic.get_header conn

let get_field (Conn {basic = (module Basic); conn}) = Basic.get_field conn

let marshal_obj (Conn {basic = (module Basic); conn}) = Basic.marshal_obj conn

let get_closure_code (Conn {basic = (module Basic); conn}) = Basic.get_closure_code conn

let set_fork_mode (Conn {basic = (module Basic); conn}) = Basic.set_fork_mode conn

[@@@ocamlformat "enable"]
