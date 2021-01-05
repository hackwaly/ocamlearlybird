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

let make_simple_value_module (type v) ?num_indexed ?get_indexed ?limit_size
    ?to_hex_string type' to_string =
  ( module struct
    type nonrec v = v

    type t += Value of v

    let extension_constructor =
      Obj.Extension_constructor.of_val (Value (Obj.magic ()))

    let is_named_container = false

    let is_indexed_container = get_indexed |> Option.is_some

    let adopt conn env ty rv =
      let%lwt pass =
        if Ctype.matches env type' ty then
          match limit_size with
          | Some limit_size ->
              let%lwt size = Debugcom.get_size conn rv in
              Lwt.return (size <= limit_size)
          | None -> Lwt.return true
        else Lwt.return false
      in
      if pass then
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
( val make_simple_value_module ~limit_size:32768 Predef.type_string
        String.escaped )

module Bytes_value = ( val make_simple_value_module ~limit_size:32768
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
      ]
    |> List.to_seq
    |> Seq.map (fun (module Value : VALUE) ->
           (Value.extension_constructor, (module Value : VALUE))) )

let find_module v =
  let ec = Obj.Extension_constructor.of_val v in
  Hashtbl.find modules ec

let adopt conn env ty rv =
  modules |> Hashtbl.to_seq_values
  |> Lwt_util.find_map_seq_s (fun (module Value : VALUE) ->
         Value.adopt conn env ty rv)

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
