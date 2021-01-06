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

module type SIMPLE_VALUE = sig
  include VALUE

  type v

  type t += Value of v
end

let rec_adopt = ref (fun _ -> assert false)

module Unknown_value = struct
  type t += Unknown

  let extension_constructor = Obj.Extension_constructor.of_val Unknown

  let is_named_container = false

  let is_indexed_container = false

  let adopt conn env ty rv =
    ignore conn;
    ignore env;
    ignore ty;
    ignore rv;
    Lwt.return (Some Unknown)

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

  let num_named _ = 0

  let list_named v =
    ignore v;
    Lwt.return []
end

module Function_value = struct
  type t += Function of Event.t

  let extension_constructor =
    Obj.Extension_constructor.of_val (Function (Obj.magic ()))

  let is_named_container = false

  let is_indexed_container = false

  let adopt conn env ty rv =
    ignore env;
    match (Ctype.repr ty).desc with
    | Types.Tarrow _ ->
        let%lwt pc = Debugcom.get_closure_code conn rv in
        let event = Symbols.find_event conn#symbols pc in
        Lwt.return (Some (Function event))
    | _ -> Lwt.return None

  let to_short_string ?(hex = false) v =
    ignore hex;
    let[@warning "-8"] (Function event) = (v [@warning "+8"]) in
    let _, line, col =
      event.ev.ev_loc.Location.loc_start |> Location.get_pos_info
    in
    let fname = event.module_.source |> Option.value ~default:"(none)" in
    Printf.sprintf "«fun» @ %s:%d:%d" (Filename.basename fname) line col

  let num_indexed v =
    ignore v;
    0

  let get_indexed v index =
    ignore v;
    ignore index;
    [%lwt assert false]

  let num_named _ = 0

  let list_named v =
    ignore v;
    Lwt.return []
end

let make_simple_value_module (type v) ?num_indexed ?get_indexed ?num_named
    ?list_named ?to_hex_string type' to_string =
  ( module struct
    type nonrec v = v

    type t += Value of v

    let extension_constructor =
      Obj.Extension_constructor.of_val (Value (Obj.magic ()))

    let is_named_container = list_named |> Option.is_some

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
      | Some num_indexed ->
          let[@warning "-8"] (Value v) = (v [@warning "+8"]) in
          num_indexed v
      | None -> 0

    let get_indexed v index =
      match get_indexed with
      | Some get_indexed ->
          let[@warning "-8"] (Value v) = (v [@warning "+8"]) in
          Lwt.return (get_indexed v index)
      | None -> [%lwt assert false]

    let num_named v =
      match num_named with
      | Some num_named ->
          let[@warning "-8"] (Value v) = (v [@warning "+8"]) in
          num_named v
      | None -> 0

    let list_named v =
      match list_named with
      | Some list_named ->
          let[@warning "-8"] (Value v) = (v [@warning "+8"]) in
          Lwt.return (list_named v)
      | None -> Lwt.return []
  end : SIMPLE_VALUE
    with type v = v )

module Int_value =
( val make_simple_value_module
        ~to_hex_string:(fun v -> Format.sprintf "%0#x" v)
        Predef.type_int Int.to_string )

module Char_value = (val make_simple_value_module Predef.type_char Char.escaped)

module String_value =
(val make_simple_value_module Predef.type_string String.escaped)

module Bytes_value = ( val make_simple_value_module Predef.type_bytes
                             ~num_indexed:(fun b -> Bytes.length b)
                             ~get_indexed:(fun b i ->
                               Int_value.Value (Bytes.get_uint8 b i))
                             (fun _ -> "«bytes»") : SIMPLE_VALUE
                         with type v = bytes )

module Float_value =
(val make_simple_value_module Predef.type_float Float.to_string)

module Bool_value =
(val make_simple_value_module Predef.type_bool Bool.to_string)

module Unit_value =
(val make_simple_value_module Predef.type_unit Unit.to_string)

module Nativeint_value =
(val make_simple_value_module Predef.type_nativeint Nativeint.to_string)

module Int32_value =
(val make_simple_value_module Predef.type_int32 Int32.to_string)

module Int64_value =
(val make_simple_value_module Predef.type_int64 Int64.to_string)

module Extension_constructor_value = ( val make_simple_value_module
                                             Predef.type_extension_constructor
                                             ~num_named:(fun _ -> 2)
                                             ~list_named:(fun v ->
                                               [
                                                 ( Ident.create_local "name",
                                                   String_value.Value
                                                     (Obj.Extension_constructor
                                                      .name v) );
                                                 ( Ident.create_local "id",
                                                   Int_value.Value
                                                     (Obj.Extension_constructor
                                                      .id v) );
                                               ])
                                             (fun _ ->
                                               "«extension constructor»")
                                         : SIMPLE_VALUE
                                         with type v = Obj.Extension_constructor
                                                       .t )

module Tuple_value = struct
  type v = {
    conn : Debugcom.conn;
    env : Env.t;
    tys : Types.type_expr list;
    rv : Debugcom.remote_value;
    pos : int;
    unboxed : bool;
  }

  type t += Tuple of v

  let extension_constructor =
    Obj.Extension_constructor.of_val (Tuple (Obj.magic ()))

  let is_named_container = true

  let is_indexed_container = false

  let to_short_string ?(hex = false) v =
    ignore hex;
    ignore v;
    "«tuple»"

  let adopt conn env ty rv =
    match (Ctype.repr ty).desc with
    | Ttuple tys ->
        Lwt.return
          (Some (Tuple { conn; env; tys; rv; pos = 0; unboxed = false }))
    | _ -> Lwt.return None

  let num_indexed v =
    ignore v;
    0

  let get_indexed v index =
    ignore v;
    ignore index;
    [%lwt assert false]

  let num_named v =
    let[@warning "-8"] (Tuple { tys; _ }) = (v [@warning "+8"]) in
    List.length tys

  let list_named v =
    let[@warning "-8"] (Tuple { conn; env; tys; rv; pos; unboxed }) =
      (v [@warning "+8"])
    in
    if unboxed then
      let%lwt value = !rec_adopt conn env (List.hd tys) rv in
      Lwt.return [ (Ident.create_local "·1", value) ]
    else
      let rec build_values values pos idx tys =
        match tys with
        | [] -> Lwt.return values
        | ty :: tys ->
            let%lwt rv = Debugcom.get_field conn rv pos in
            let ident = Ident.create_local ("·" ^ string_of_int (idx + 1)) in
            let%lwt value = !rec_adopt conn env ty rv in
            build_values ((ident, value) :: values) (pos + 1) (idx + 1) tys
      in
      let%lwt values = build_values [] pos 0 tys in
      let values = List.rev values in
      Lwt.return values
end

module List_nil_value = struct
  type t += List_nil

  let extension_constructor = Obj.Extension_constructor.of_val List_nil

  let is_named_container = false

  let is_indexed_container = false

  let to_short_string ?(hex = false) v =
    ignore hex;
    ignore v;
    "«list.nil»"

  let adopt conn env ty rv =
    ignore conn;
    ignore env;
    match (Ctype.repr ty).desc with
    | Tconstr (path, [ _ ], _)
      when Path.same path Predef.path_list && not (Debugcom.is_block rv) ->
        Lwt.return (Some List_nil)
    | _ -> Lwt.return None

  let num_indexed v =
    ignore v;
    0

  let get_indexed v index =
    ignore v;
    ignore index;
    [%lwt assert false]

  let num_named _ = 0

  let list_named v =
    ignore v;
    Lwt.return []
end

module List_cons_value = struct
  type v = {
    conn : Debugcom.conn;
    env : Env.t;
    ty : Types.type_expr;
    rv : Debugcom.remote_value;
  }

  type t += List of v

  let extension_constructor =
    Obj.Extension_constructor.of_val (List (Obj.magic ()))

  let is_named_container = true

  let is_indexed_container = false

  let to_short_string ?(hex = false) v =
    ignore hex;
    ignore v;
    "«list.cons»"

  let adopt conn env ty rv =
    match (Ctype.repr ty).desc with
    | Tconstr (path, [ _ ], _)
      when Path.same path Predef.path_list && Debugcom.is_block rv ->
        Lwt.return (Some (List { conn; env; ty; rv }))
    | _ -> Lwt.return None

  let num_indexed v =
    ignore v;
    0

  let get_indexed v index =
    ignore v;
    ignore index;
    [%lwt assert false]

  let num_named _ = 2

  let list_named v =
    let[@warning "-8"] (List { conn; env; ty; rv }) = (v [@warning "+8"]) in
    let[@warning "-8"] (Types.Tconstr (_, [ elt_ty ], _)) =
      ((Ctype.repr ty).desc [@warning "+8"])
    in
    let make_variable name pos ty =
      let%lwt rv = Debugcom.get_field conn rv pos in
      let name = Ident.create_local name in
      let%lwt value = !rec_adopt conn env ty rv in
      Lwt.return (name, value)
    in
    let%lwt hd = make_variable "·hd" 0 elt_ty in
    let%lwt tl = make_variable "·tl" 1 ty in
    Lwt.return [ hd; tl ]
end

module Array_value = struct
  type v = {
    conn : Debugcom.conn;
    env : Env.t;
    elt_ty : Types.type_expr;
    rv : Debugcom.remote_value;
    len : int;
  }

  type t += Array of v

  let extension_constructor =
    Obj.Extension_constructor.of_val (Array (Obj.magic ()))

  let is_named_container = true

  let is_indexed_container = true

  let to_short_string ?(hex = false) v =
    ignore hex;
    ignore v;
    "«array»"

  let adopt conn env ty rv =
    match (Ctype.repr ty).desc with
    | Tconstr (path, [ elt_ty ], _)
      when Path.same path Predef.path_array && Debugcom.is_block rv ->
        let%lwt len = Debugcom.get_size conn rv in
        Lwt.return (Some (Array { conn; env; elt_ty; rv; len }))
    | _ -> Lwt.return None

  let num_indexed v =
    let[@warning "-8"] (Array { len; _ }) = (v [@warning "+8"]) in
    len

  let get_indexed v index =
    let[@warning "-8"] (Array { conn; env; elt_ty; rv; _ }) =
      (v [@warning "+8"])
    in
    let%lwt rv' = Debugcom.get_field conn rv index in
    !rec_adopt conn env elt_ty rv'

  let num_named _ = 1

  let list_named v =
    let[@warning "-8"] (Array { len; _ }) = (v [@warning "+8"]) in
    Lwt.return [ (Ident.create_local "length", Int_value.Value len) ]
end

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
        (module Nativeint_value : VALUE);
        (module Int32_value : VALUE);
        (module Int64_value : VALUE);
        (module Extension_constructor_value : VALUE);
        (module Function_value : VALUE);
        (module Tuple_value : VALUE);
        (module List_cons_value : VALUE);
        (module List_nil_value : VALUE);
        (module Array_value : VALUE);
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
  with Not_found -> Lwt.return Unknown_value.Unknown

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

let num_named v =
  let (module Value : VALUE) = find_module v in
  Value.num_named v

let list_named v =
  let (module Value : VALUE) = find_module v in
  Value.list_named v

let () = rec_adopt := adopt
