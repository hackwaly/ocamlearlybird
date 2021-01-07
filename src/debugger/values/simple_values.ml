open Value_basic
open Misc_values

module type SIMPLE_VALUE = sig
  include VALUE

  type v

  type t += Value of v
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
                                                   Raw_string_value.Raw_string
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
