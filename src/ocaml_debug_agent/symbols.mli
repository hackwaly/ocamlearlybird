open Debug_types

module Module : sig
  type t

  val id : t -> string

  val frag : t -> int

  val source : t -> string option

  val source_content : t -> string Lwt.t

  val source_line_start : t -> int -> int Lwt.t

  val source_line_length : t -> int -> int Lwt.t

  val line_column_to_cnum : t -> int -> int -> int Lwt.t

  val to_seq_events : t -> Instruct.debug_event Seq.t

  val find_event : t -> int -> int -> Instruct.debug_event Lwt.t
end

type t

val create : unit -> t

val version : t -> int

val source_dirs : t -> string list

val commit : t -> Debugcom.conn -> unit Lwt.t

val load : t -> frag:int -> string -> unit Lwt.t

val to_seq_modules : t -> Module.t Seq.t

val to_seq_events : t -> Instruct.debug_event Seq.t

val find_event : t -> pc -> Instruct.debug_event

val find_module : t -> string -> Module.t

val find_module_by_source : t -> string -> Module.t Lwt.t
