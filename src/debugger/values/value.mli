type t

val adopt :
  Debugcom.conn -> Env.t -> Types.type_expr -> Debugcom.remote_value -> t Lwt.t

val is_indexed_container : t -> bool

val to_short_string : ?hex:bool -> t -> string

val get_indexed : t -> int -> t Lwt.t

val num_indexed : t -> int

val num_named : t -> int

val list_named : t -> (string * t) list Lwt.t
