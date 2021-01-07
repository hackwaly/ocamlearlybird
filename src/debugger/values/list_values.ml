open Value_basic

module List_nil_value = struct
  type t += List_nil

  let extension_constructor = Obj.Extension_constructor.of_val List_nil

  let is_indexed_container = false

  let to_short_string ?(hex = false) v =
    ignore hex;
    ignore v;
    "[]"

  let adopt conn env ty rv =
    ignore conn;
    ignore env;
    match (Ctype.repr ty).desc with
    | Tconstr (_, [ aty1 ], _)
      when Ctype.matches env (Predef.type_list aty1) ty
           && not (Debugcom.is_block rv) ->
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

  let is_indexed_container = false

  let to_short_string ?(hex = false) v =
    ignore hex;
    ignore v;
    "‹hd› :: ‹tl›"

  let adopt conn env ty rv =
    match (Ctype.repr ty).desc with
    | Tconstr (_, [ aty1 ], _)
      when Ctype.matches env (Predef.type_list aty1) ty && Debugcom.is_block rv
      ->
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
      let%lwt value = !rec_adopt conn env ty rv in
      Lwt.return (name, value)
    in
    let%lwt hd = make_variable "‹hd›" 0 elt_ty in
    let%lwt tl = make_variable "‹tl›" 1 ty in
    Lwt.return [ hd; tl ]
end
