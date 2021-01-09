open Value_basic
open Misc_values

module Module_value = struct
  include Impl_base_value

  type desc = {
    conn : Debugcom.conn;
    env : Env.t;
    rv : Debugcom.remote_value;
    path : Path.t;
    is_packaged : Path.t option;
  }

  type t += Module of desc

  let extension_constructor =
    Obj.Extension_constructor.of_val (Module (Obj.magic ()))

  let to_short_string ?(hex = false) v =
    ignore hex;
    let[@warning "-8"] (Module { rv; is_packaged; path; _ }) =
      (v [@warning "+8"])
    in
    if is_packaged |> Option.is_some then
      Format.sprintf "(module %s : %s)" (Util.Path.to_string path)
        (Util.Path.to_string (is_packaged |> Option.get))
    else if Debugcom.is_block rv then "«module»"
    else "«module.unbound»"

  let adopt conn env ty rv =
    match (Ctype.repr ty).desc with
    | Tpackage (path, [], []) -> (
        match env |> Env.find_modtype_expansion path with
        | modtype ->
            let mid =
              Ident.create_persistent
                (Printf.sprintf "M_%04x"
                   (Float.to_int (Sys.time () *. 1e9) mod 0x10000))
            in
            let env = env |> Env.add_module mid Types.Mp_present modtype in
            Lwt.return
              (Some
                 (Module
                    {
                      conn;
                      env;
                      rv;
                      path = Path.Pident mid;
                      is_packaged = Some path;
                    }))
        | exception _ -> Lwt.return None )
    | _ -> Lwt.return None

  let num_named v =
    let[@warning "-8"] (Module { rv; _ }) = (v [@warning "+8"]) in
    if Debugcom.is_block rv then -1 else 0

  (* WTF: Env.fold_values Not exposed *)
  let list_named v =
    let[@warning "-8"] (Module { conn; env; path; rv; _ }) =
      (v [@warning "+8"])
    in
    if not (Debugcom.is_block rv) then Lwt.return []
    else
      let val_pos_list, env' = Util.Env.list_value_pos env path in
      let%lwt variables =
        val_pos_list
        |> Lwt_list.filter_map_s (fun (kind, name, pos) ->
               try%lwt
                 let%lwt rv' = Debugcom.get_field conn rv pos in
                 let path' = Path.Pdot (path, name) in
                 match kind with
                 | `Value ->
                     let decl = env' |> Env.find_value path' in
                     let%lwt value = !rec_adopt conn env decl.val_type rv' in
                     Lwt.return (Some (name, value))
                 | `Module ->
                     let value =
                       Module
                         {
                           conn;
                           env = env';
                           rv;
                           path = path';
                           is_packaged = None;
                         }
                     in
                     Lwt.return (Some (name, value))
               with Not_found -> Lwt.return None)
      in
      Lwt.return variables
end
