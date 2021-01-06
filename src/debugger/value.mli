type t

val adopt :
  Debugcom.conn -> Env.t -> Types.type_expr -> Debugcom.remote_value -> t Lwt.t

val is_named_container : t -> bool

val is_indexed_container : t -> bool

val to_short_string : ?hex:bool -> t -> string

val get_indexed : t -> int -> t Lwt.t

val num_indexed : t -> int

val get_named : t -> (Ident.t * t) list Lwt.t
