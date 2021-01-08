open Value_basic

module Record_value = struct
  include Impl_base_value

  type desc = {
    conn : Debugcom.conn;
    env : Env.t;
    unboxed : bool;
    rv : Debugcom.remote_value;
    pos : int;
    labels : (string * Types.type_expr) list;
  }

  type t += Record of desc

  let extension_constructor =
    Obj.Extension_constructor.of_val (Record (Obj.magic ()))

  let to_short_string ?(hex = false) _ =
    ignore hex;
    "{…}"

  let adopt conn env ty rv =
    match (Ctype.repr ty).desc with
    | Types.Tconstr (path, ty_args, _) -> (
        match Env.find_type path env with
        | { type_kind = Type_record (labels, rep); type_params; _ } ->
            let unboxed =
              match rep with Record_unboxed _ -> true | _ -> false
            in
            let pos = match rep with Record_extension _ -> 1 | _ -> 0 in
            let labels =
              labels
              |> List.map (fun lbl ->
                     let id = lbl.Types.ld_id in
                     let ty =
                       try Ctype.apply env type_params lbl.ld_type ty_args
                       with Ctype.Cannot_apply -> lbl.ld_type
                     in
                     (Ident.name id, ty))
            in
            Lwt.return (Some (Record { conn; env; unboxed; rv; pos; labels }))
        | _ | (exception Not_found) -> Lwt.return None )
    | _ -> Lwt.return None

  let num_named v =
    let[@warning "-8"] (Record { labels; _ }) = (v [@warning "+8"]) in
    List.length labels

  let list_named v =
    let[@warning "-8"] (Record { conn; env; rv; unboxed; pos; labels; _ }) =
      (v [@warning "+8"])
    in
    if unboxed then
      let id, ty = List.hd labels in
      let%lwt value = !rec_adopt conn env ty rv in
      Lwt.return [ (id, value) ]
    else
      labels
      |> Lwt_list.mapi_s (fun i (id, ty) ->
             let%lwt rv' = Debugcom.get_field conn rv (pos + i) in
             let%lwt value = !rec_adopt conn env ty rv' in
             Lwt.return (id, value))
end
