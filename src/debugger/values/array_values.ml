open Value_basic
open Simple_values

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

  let is_indexed_container = true

  let adopt conn env ty rv =
    match (Ctype.repr ty).desc with
    | Tconstr (_, [ elt_ty ], _)
      when Ctype.matches env (Predef.type_array elt_ty) ty
           && Debugcom.is_block rv ->
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
    Lwt.return [ ("‹length›", Int_value.Value len) ]

  let to_short_string ?(hex = false) v =
    ignore hex;
    let n = num_indexed v in
    if n = 0 then "[||]"
    else if n = 1 then "[|‹1›|]"
    else if n = 2 then "[|‹1›; ‹2›|]"
    else if n = 3 then "[|‹1›; ‹2›; ‹3›|]"
    else "[|‹1›; ‹2›; …|]"
end
