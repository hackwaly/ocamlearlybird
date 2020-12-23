open Remote_debugger

module Module : sig
  type t = {
    frag : int;
    id : string;
    resolved_source : string option;
    events : Instruct.debug_event array;
  }

  val find_event : t -> int -> int -> Instruct.debug_event Lwt.t
end

type t

val create : unit -> t

val version : t -> int

val source_dirs : t -> string list

val commit : t -> (module Remote_debugger.S) -> conn -> unit Lwt.t

val load : t -> frag:int -> string -> unit Lwt.t

val to_seq_modules : t -> Module.t Seq.t

val to_seq_events : t -> Instruct.debug_event Seq.t

val find_event : t -> pc -> Instruct.debug_event

val find_module : t -> string -> Module.t

val find_module_by_source : t -> string -> Module.t Lwt.t
