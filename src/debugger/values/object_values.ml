open Value_basic

module Object_value = struct
  type desc = {
    conn : Debugcom.conn;
    env : Env.t;
    rv : Debugcom.remote_value;
    field_tys : (string * Types.type_expr) list;
  }

  type t += Object of desc

  let extension_constructor =
    Obj.Extension_constructor.of_val (Object (Obj.magic ()))

  let is_indexed_container = false

  let to_short_string ?(hex = false) v =
    ignore hex;
    ignore v;
    "«object»"

  let adopt conn env ty rv =
    ignore conn;
    ignore env;
    ignore rv;
    match (Ctype.repr ty).desc with
    | Tobject (fields_ty, _) ->
        let rec aux r fields_ty =
          match fields_ty.Types.desc with
          | Types.Tfield (name, _, ty', fields_ty) ->
              aux ((name, ty') :: r) fields_ty
          | Tnil -> r
          | _ -> assert false
        in
        let field_tys = aux [] fields_ty in
        Lwt.return (Some (Object { conn; env; rv; field_tys }))
    | _ -> Lwt.return None

  let num_indexed _ = 0

  let num_named v =
    let[@warning "-8"] (Object { field_tys; _ }) = (v [@warning "+8"]) in
    List.length field_tys

  let get_indexed v index =
    ignore v;
    ignore index;
    [%lwt assert false]

  let list_named v =
    let[@warning "-8"] (Object { conn; env; rv; field_tys; _ }) =
      (v [@warning "+8"])
    in
    let%lwt table = Debugcom.get_field conn rv 0 in
    let%lwt num_methods = Debugcom.get_field conn table 0 in
    let%lwt num_methods = Debugcom.marshal_obj conn num_methods in
    let find_method name =
      let left = ref 0 in
      let right = ref num_methods in
      let tag = CamlinternalOO.public_method_label name in
      while%lwt !left < !right do
        let middle = (!left + !right) / 2 in
        let%lwt tag' = Debugcom.get_field conn table (2 + (2 * middle) + 1) in
        let%lwt tag' = Debugcom.marshal_obj conn tag' in
        if tag <= tag' then right := middle else left := middle + 1;
        Lwt.return ()
      done;%lwt
      let%lwt method' = Debugcom.get_field conn table (2 + (2 * !left)) in
      Lwt.return method'
    in
    let%lwt variables =
      field_tys
      |> Lwt_list.map_s (fun (name, ty) ->
             let ty = Ctype.newty (Types.Tarrow (Nolabel, Predef.type_unit, ty, Cok)) in
             let ident = Ident.create_local name in
             let%lwt meth = find_method name in
             let%lwt value = !rec_adopt conn env ty meth in
             Log.debug (fun m -> m "9");%lwt
             Lwt.return (ident, value))
    in
    Lwt.return variables
end
