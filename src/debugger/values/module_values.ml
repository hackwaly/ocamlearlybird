open Value_basic
open Misc_values

module Module_value = struct
  include Impl_base_value

  type desc = {
    conn : Debugcom.conn;
    env : Env.t;
    rv : Debugcom.remote_value;
    path : Path.t option;
    modtype : Types.module_type;
  }

  type t += Module of desc

  let extension_constructor =
    Obj.Extension_constructor.of_val (Module (Obj.magic ()))

  let to_short_string ?(hex = false) v =
    ignore hex;
    let[@warning "-8"] (Module { rv; _ }) = (v [@warning "+8"]) in
    if Debugcom.is_block rv then "«module»" else "«module.unbound»"

  let adopt conn env ty rv =
    match (Ctype.repr ty).desc with
    | Tpackage (path, [], []) -> (
        match env |> Env.find_modtype_expansion path with
        | modtype ->
            Lwt.return (Some (Module { conn; env; rv; path = None; modtype }))
        | exception _ -> Lwt.return None )
    | _ -> Lwt.return None

  let num_named _ = -1

  (* WTF: Env.fold_values Not exposed *)
  let list_named v =
    let[@warning "-8"] (Module { conn; env; modtype; path; rv }) =
      (v [@warning "+8"])
    in
    if not (Debugcom.is_block rv) then Lwt.return []
    else
      let val_pos_list, env' = Util.Env.list_value_pos modtype in
      let env' = match path with Some _ -> env | None -> env' in
      let make_path name =
        match path with
        | Some path -> Path.Pdot (path, name)
        | None -> Path.Pdot (Path.Pident Util.Env.dummy_module_id, name)
      in
      let%lwt variables =
        val_pos_list
        |> Lwt_list.filter_map_s (fun (name, pos) ->
               try%lwt
                 let path = make_path name in
                 let decl = env' |> Env.find_value path in
                 let%lwt rv' = Debugcom.get_field conn rv pos in
                 let%lwt value = !rec_adopt conn env decl.val_type rv' in
                 Lwt.return (Some (name, value))
               with Not_found -> Lwt.return None)
      in
      Lwt.return variables
end
