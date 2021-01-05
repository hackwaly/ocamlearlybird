module Log = Log

type pc = Pc.t = { frag : int; pos : int }

type protocol_version = Debugcom.protocol_version = OCaml_400 | OCaml_410

type options = {
  protocol_version : protocol_version; [@default OCaml_410]
  debug_socket : Lwt_unix.file_descr;
  symbols_file : string;
  yield_point : int; [@default 1024]
}
[@@deriving make]

type status =
  | Unstarted
  | Running
  | Stopped of { time : int64; breakpoint : bool }
  | Exited of { time : int64; uncaught_exc : bool }

type t

module Module = Module
module Event = Event
module Frame = Frame
module Value = Value

val create : options -> t

val symbols_did_update_event : t -> unit Lwt_react.E.t

val to_seq_modules : t -> Module.t Seq.t

val find_module : t -> string -> Module.t

val find_module_by_source : t -> string -> Module.t

val status_signal : t -> status Lwt_react.S.t

val set_breakpoint : t -> pc -> unit

val remove_breakpoint : t -> pc -> unit

val run : t -> unit

val step_in : t -> unit

val step_out : t -> unit

val step_over : t -> unit

val pause : t -> unit

val stop : t -> unit

val initial_frame : t -> Frame.t Lwt.t

val up_frame : t -> Frame.t -> Frame.t option Lwt.t

val set_frame : t -> Frame.t -> unit Lwt.t

val start : t -> unit Lwt.t

val frame_variables : t -> Frame.t -> [`Stack | `Heap] -> (Ident.t * Value.t) list Lwt.t
