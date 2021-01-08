type t = ..

type t += Unknown

module type VALUE = sig
  val extension_constructor : Obj.Extension_constructor.t

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

  val list_named : t -> (string * t) list Lwt.t
end

let rec_adopt :
    (Debugcom.conn ->
    Env.t ->
    Types.type_expr ->
    Debugcom.remote_value ->
    t Lwt.t)
    ref =
  ref (fun _ -> assert false)

let rec_find_module : (t -> (module VALUE)) ref = ref (fun _ -> assert false)

let to_short_string ?(hex = false) v =
  let (module Value : VALUE) = !rec_find_module v in
  Value.to_short_string ~hex v

let is_indexed_container v =
  let (module Value : VALUE) = !rec_find_module v in
  Value.is_indexed_container

let get_indexed v index =
  let (module Value : VALUE) = !rec_find_module v in
  Value.get_indexed v index

let num_indexed v =
  let (module Value : VALUE) = !rec_find_module v in
  Value.num_indexed v

let num_named v =
  let (module Value : VALUE) = !rec_find_module v in
  Value.num_named v

let list_named v =
  let (module Value : VALUE) = !rec_find_module v in
  Value.list_named v
