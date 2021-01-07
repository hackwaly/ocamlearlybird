open Value_basic

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

  let is_indexed_container = false

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

  let to_short_string ?(hex = false) v =
    ignore hex;
    let n = num_named v in
    if n = 0 then "()"
    else if n = 1 then "‹1›"
    else if n == 2 then "(‹1›, ‹2›)"
    else if n == 3 then "(‹1›, ‹2›, ‹3›)"
    else "(‹1›, ‹2›, …)"

  let list_named v =
    let[@warning "-8"] (Tuple { conn; env; tys; rv; pos; unboxed }) =
      (v [@warning "+8"])
    in
    if unboxed then
      let%lwt value = !rec_adopt conn env (List.hd tys) rv in
      Lwt.return [ ("‹1›", value) ]
    else
      let rec build_values values pos idx tys =
        match tys with
        | [] -> Lwt.return values
        | ty :: tys ->
            let%lwt rv = Debugcom.get_field conn rv pos in
            let name = "‹" ^ string_of_int (idx + 1) ^ "›" in
            let%lwt value = !rec_adopt conn env ty rv in
            build_values ((name, value) :: values) (pos + 1) (idx + 1) tys
      in
      let%lwt values = build_values [] pos 0 tys in
      let values = List.rev values in
      Lwt.return values
end
