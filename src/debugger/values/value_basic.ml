type t = ..

module type VALUE = sig
  val extension_constructor : Obj.Extension_constructor.t

  val is_named_container : bool

  val is_indexed_container : bool

  val adopt :
    Debugcom.conn ->
    Env.t ->
    Types.type_expr ->
    Debugcom.remote_value ->
    t option Lwt.t

  val to_short_string : ?hex:bool -> t -> string

  val num_indexed : t -> int

  val get_indexed : t -> int -> t Lwt.t

  val num_named : t -> int

  val list_named : t -> (Ident.t * t) list Lwt.t
end

let rec_adopt : (Debugcom.conn -> Env.t -> Types.type_expr -> Debugcom.remote_value -> t Lwt.t) ref = ref (fun _ -> assert false)
