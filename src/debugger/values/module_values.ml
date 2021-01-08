open Value_basic
open Misc_values

module Module_value = struct
  include Impl_base_value

  type desc = {
    conn : Debugcom.conn;
    env : Env.t;
    rv : Debugcom.remote_value;
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
        | modtype -> Lwt.return (Some (Module { conn; env; rv; modtype }))
        | exception _ -> Lwt.return None )
    | _ -> Lwt.return None

  let num_named _ = -1

  (* WTF: Env.fold_values Not exposed *)
  let list_named v =
    let[@warning "-8"] (Module { conn; env; modtype; rv }) =
      (v [@warning "+8"])
    in
    if not (Debugcom.is_block rv) then Lwt.return []
    else
      let mid = Ident.create_persistent "Temp_ywwofnzftu" in
      Log.debug (fun m -> m "1");%lwt
      let env' = Env.empty |> Env.add_module mid Types.Mp_present modtype in
      Log.debug (fun m -> m "2");%lwt
      let val_names =
        env' |> Env.extract_values (Some (Longident.Lident (Ident.name mid)))
      in
      Log.debug (fun m -> m "3");%lwt
      let%lwt variables =
        val_names
        |> Lwt_list.filter_map_s (fun name ->
               try%lwt
                 let path = Path.Pdot (Path.Pident mid, name) in
                 Log.debug (fun m -> m "4");%lwt
                 let decl = env' |> Env.find_value path in
                 Log.debug (fun m -> m "5");%lwt
                 let addr = env' |> Env.find_value_address path in
                 Log.debug (fun m -> m "6");%lwt
                 let pos =
                   match addr with
                   | Adot (Aident id, pos) when Ident.same id mid -> pos
                   | _ -> raise Not_found
                 in
                 Log.debug (fun m -> m "7 pos: %d" pos);%lwt
                 let%lwt rv' = Debugcom.get_field conn rv pos in
                 Log.debug (fun m -> m "8");%lwt
                 let%lwt value = !rec_adopt conn env decl.val_type rv' in
                 Log.debug (fun m -> m "9");%lwt
                 Lwt.return (Some (name, value))
               with Not_found -> Lwt.return None)
      in
      Lwt.return variables
end
