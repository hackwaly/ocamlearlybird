type t

val create : unit -> t

val did_update_event : t -> unit Lwt_react.E.t

val to_seq_modules : t -> Debuginfo.module_ Seq.t

val find_module_by_source : t -> string -> Debuginfo.module_

val find_module : t -> string -> Debuginfo.module_

val find_event : t -> Pc.t -> Debuginfo.event

val load : t -> int -> string -> unit Lwt.t

val commit : t -> (Pc.t Seq.t -> unit Lwt.t) -> unit Lwt.t

