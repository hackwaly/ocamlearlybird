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
end

module type SIMPLE_VALUE = sig
  include VALUE

  type v

  type t += Value of v
end

module Unknown_value = struct
  type t += Value

  let extension_constructor = Obj.Extension_constructor.of_val Value

  let is_named_container = false

  let is_indexed_container = false

  let adopt conn env ty rv =
    ignore conn;
    ignore env;
    ignore ty;
    ignore rv;
    Lwt.return (Some Value)

  let to_short_string ?(hex = false) v =
    ignore hex;
    ignore v;
    "«unknown»"

  let num_indexed v =
    ignore v;
    0

  let get_indexed v index =
    ignore v;
    ignore index;
    [%lwt assert false]
end

module Function_value = struct
  type t += Value of Event.t

  let extension_constructor =
    Obj.Extension_constructor.of_val (Value (Obj.magic ()))

  let is_named_container = false

  let is_indexed_container = false

  let adopt conn env ty rv =
    ignore env;
    match (Ctype.repr ty).desc with
    | Types.Tarrow _ ->
        let%lwt pc = Debugcom.get_closure_code conn rv in
        let event = Symbols.find_event conn#symbols pc in
        Lwt.return (Some (Value event))
    | _ -> Lwt.return None

  let to_short_string ?(hex = false) v =
    ignore hex;
    let [@warning "-8"] (Value event) = v [@warning "+8"] in
    let (_, line, col) = event.ev.ev_loc.Location.loc_start |> Location.get_pos_info in
    let fname = event.module_.source |> Option.value ~default:"(none)" in
    Printf.sprintf "«fun» @ %s:%d:%d" (Filename.basename fname) line col

  let num_indexed v =
    ignore v;
    0

  let get_indexed v index =
    ignore v;
    ignore index;
    [%lwt assert false]
end

let make_simple_value_module (type v) ?num_indexed ?get_indexed ?to_hex_string
    type' to_string =
  ( module struct
    type nonrec v = v

    type t += Value of v

    let extension_constructor =
      Obj.Extension_constructor.of_val (Value (Obj.magic ()))

    let is_named_container = false

    let is_indexed_container = get_indexed |> Option.is_some

    let adopt conn env ty rv =
      if Ctype.matches env type' ty then
        let%lwt v = Debugcom.marshal_obj conn rv in
        Lwt.return (Some (Value v))
      else Lwt.return None

    let to_short_string ?(hex = false) v =
      match v with
      | Value v ->
          let to_string =
            if hex then to_hex_string |> Option.value ~default:to_string
            else to_string
          in
          to_string v
      | _ -> assert false

    let num_indexed v =
      match num_indexed with
      | Some num_indexed -> (
          match v with Value v -> num_indexed v | _ -> assert false )
      | None -> 0

    let get_indexed v index =
      match get_indexed with
      | Some get_indexed -> (
          match v with
          | Value v -> Lwt.return (get_indexed v index)
          | _ -> [%lwt assert false] )
      | None -> [%lwt assert false]
  end : SIMPLE_VALUE
    with type v = v )

module Int_value =
( val make_simple_value_module
        ~to_hex_string:(fun v -> Format.sprintf "%0#x" v)
        Predef.type_int Int.to_string )

module Char_value = (val make_simple_value_module Predef.type_char Char.escaped)

module String_value =
(val make_simple_value_module Predef.type_string String.escaped)

module Bytes_value = ( val make_simple_value_module
                             ~num_indexed:(fun b -> Bytes.length b)
                             ~get_indexed:(fun b i ->
                               Int_value.Value (Bytes.get_uint8 b i))
                             Predef.type_bytes
                             (fun _ -> "«bytes»") : SIMPLE_VALUE
                         with type v = bytes )

module Float_value =
(val make_simple_value_module Predef.type_float Float.to_string)

module Bool_value =
(val make_simple_value_module Predef.type_bool Bool.to_string)

module Unit_value =
(val make_simple_value_module Predef.type_unit Unit.to_string)

let modules =
  Hashtbl.of_seq
    ( [
        (module Int_value : VALUE);
        (module Char_value : VALUE);
        (module String_value : VALUE);
        (module Bytes_value : VALUE);
        (module Float_value : VALUE);
        (module Bool_value : VALUE);
        (module Unit_value : VALUE);
        (module Function_value : VALUE);
      ]
    |> List.to_seq
    |> Seq.map (fun (module Value : VALUE) ->
           (Value.extension_constructor, (module Value : VALUE))) )

let find_module v =
  try
    let ec = Obj.Extension_constructor.of_val v in
    Hashtbl.find modules ec
  with Not_found -> (module Unknown_value : VALUE)

let adopt conn env ty rv =
  try%lwt
    modules |> Hashtbl.to_seq_values
    |> Lwt_util.find_map_seq_s (fun (module Value : VALUE) ->
           Value.adopt conn env ty rv)
  with Not_found -> Lwt.return Unknown_value.Value

let to_short_string ?(hex = false) v =
  let (module Value : VALUE) = find_module v in
  Value.to_short_string ~hex v

let is_named_container v =
  let (module Value : VALUE) = find_module v in
  Value.is_named_container

let is_indexed_container v =
  let (module Value : VALUE) = find_module v in
  Value.is_indexed_container

let get_indexed v index =
  let (module Value : VALUE) = find_module v in
  Value.get_indexed v index

let num_indexed v =
  let (module Value : VALUE) = find_module v in
  Value.num_indexed v
