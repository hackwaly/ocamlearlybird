open Value_basic
open Misc_values

module Lazy_value = struct
  include Impl_base_value

  type t += Lazy of t

  let extension_constructor =
    Obj.Extension_constructor.of_val (Lazy (Obj.magic ()))

  let to_short_string ?(hex = false) _ =
    ignore hex;
    "«lazy»"

  let adopt conn env ty rv =
    match (Ctype.repr ty).desc with
    | Tconstr (_, [ aty1 ], _)
      when Ctype.matches env (Predef.type_lazy_t aty1) ty ->
        let%lwt tag = Debugcom.get_tag conn rv in
        if tag = Obj.lazy_tag then
          let%lwt rv_f = Debugcom.get_field conn rv 0 in
          let ty_f =
            Ctype.newty (Types.Tarrow (Nolabel, Predef.type_unit, aty1, Cok))
          in
          let%lwt value = !rec_adopt conn env ty_f rv_f in
          Lwt.return (Some (Lazy value))
        else Lwt.return None
    | _ -> Lwt.return None

  let num_named _ = 1

  let list_named v =
    let[@warning "-8"] (Lazy f) = (v [@warning "+8"]) in
    Lwt.return [ ("‹fun›", f) ]
end

module Lazy_fourced_value = struct
  include Impl_base_value

  type t += Forced of t

  let extension_constructor =
    Obj.Extension_constructor.of_val (Forced (Obj.magic ()))

  let to_short_string ?(hex = false) _ =
    ignore hex;
    "«lazy.is_val»"

  let adopt conn env ty rv =
    match (Ctype.repr ty).desc with
    | Tconstr (_, [ aty1 ], _)
      when Ctype.matches env (Predef.type_lazy_t aty1) ty ->
        let%lwt tag = Debugcom.get_tag conn rv in
        if tag <> Obj.lazy_tag then
          let%lwt rv = Debugcom.get_field conn rv 0 in
          let%lwt value = !rec_adopt conn env aty1 rv in
          Lwt.return (Some (Forced value))
        else Lwt.return None
    | _ -> Lwt.return None

  let num_named _ = 1

  let list_named v =
    let[@warning "-8"] (Forced v) = (v [@warning "+8"]) in
    Lwt.return [ ("‹val›", v) ]
end
