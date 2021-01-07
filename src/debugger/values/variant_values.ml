open Value_basic
open Tuple_values
open Record_values

module Variant_value = struct
  type v = { name : string; payload : t option; embed : bool }

  type t += Variant of v

  let extension_constructor =
    Obj.Extension_constructor.of_val (Variant (Obj.magic ()))

  let to_short_string ?(hex = false) v =
    ignore hex;
    let[@warning "-8"] (Variant { name; payload; embed }) =
      (v [@warning "+8"])
    in
    let payload_str =
      match payload with
      | None -> ""
      | Some v ->
          " " ^ if embed then Value_basic.to_short_string v else "‹1›"
    in
    name ^ payload_str

  let adopt conn env ty rv =
    match (Ctype.repr ty).desc with
    | Tvariant row -> (
        let row = Btype.row_repr row in
        if Debugcom.is_block rv then
          let%lwt tag = Debugcom.get_field conn rv 0 in
          let%lwt tag = Debugcom.marshal_obj conn tag in
          let rec find = function
            | (l, f) :: fields ->
                if Btype.hash_variant l = tag then
                  match Btype.row_field_repr f with
                  | Rpresent (Some ty) | Reither (_, [ ty ], _, _) ->
                      Some (l, ty)
                  | _ -> find fields
                else find fields
            | [] -> None
          in
          match find row.row_fields with
          | Some (l, ty') ->
              let%lwt rv' = Debugcom.get_field conn rv 1 in
              let%lwt payload = !rec_adopt conn env ty' rv' in
              Lwt.return
                (Some
                   (Variant
                      { name = "`" ^ l; payload = Some payload; embed = false }))
          | None -> Lwt.return None
        else
          let%lwt tag = Debugcom.marshal_obj conn rv in
          let rec find = function
            | (l, _) :: fields ->
                if Btype.hash_variant l = tag then Some l else find fields
            | [] -> None
          in
          match find row.row_fields with
          | Some l ->
              Lwt.return
                (Some
                   (Variant { name = "`" ^ l; payload = None; embed = false }))
          | None -> Lwt.return None )
    | Tconstr (path, ty_args, _) -> (
        match Env.find_type path env with
        | exception Not_found -> Lwt.return None
        | { type_kind = Type_open; _ } ->
            let%lwt tag = Debugcom.get_tag conn rv in
            let%lwt slot =
              if tag <> 0 then Lwt.return rv else Debugcom.get_field conn rv 0
            in
            let%lwt id = Debugcom.get_field conn slot 0 in
            let%lwt id = Debugcom.marshal_obj conn id in
            let longid = Parse.longident (Lexing.from_string id) in
            let%lwt payload =
              match Env.find_constructor_by_name longid env with
              | cstr_decl ->
                  if cstr_decl.cstr_args = [] then Lwt.return None
                  else
                    Lwt.return
                      (Some
                         (Tuple_value.Tuple
                            {
                              conn;
                              env;
                              rv;
                              pos = 1;
                              tys = cstr_decl.cstr_args;
                              unboxed = cstr_decl.cstr_inlined |> Option.is_some;
                            }))
              | exception Not_found -> Lwt.return None
            in
            Lwt.return (Some (Variant { name = id; payload; embed = true }))
        | {
         type_kind = Type_variant constr_list;
         type_unboxed = { unboxed; _ };
         type_params;
         _;
        } -> (
            let%lwt tag =
              if unboxed then Lwt.return Types.Cstr_unboxed
              else if not (Debugcom.is_block rv) then
                let%lwt tag = Debugcom.marshal_obj conn rv in
                Lwt.return (Types.Cstr_constant tag)
              else
                let%lwt tag = Debugcom.get_tag conn rv in
                Lwt.return (Types.Cstr_block tag)
            in
            match Datarepr.find_constr_by_tag tag constr_list with
            | constr ->
                let%lwt payload =
                  match constr.cd_args with
                  | Types.Cstr_tuple [] -> Lwt.return None
                  | Types.Cstr_tuple tys ->
                      let tys =
                        tys
                        |> List.map (fun ty ->
                               try Ctype.apply env type_params ty ty_args
                               with Ctype.Cannot_apply -> ty)
                      in
                      Lwt.return
                        (Some
                           (Tuple_value.Tuple
                              { conn; env; rv; pos = 0; tys; unboxed }))
                  | Cstr_record labels ->
                      let labels =
                        labels
                        |> List.map (fun lbl ->
                               let id = lbl.Types.ld_id in
                               let ty =
                                 try
                                   Ctype.apply env type_params lbl.ld_type
                                     ty_args
                                 with Ctype.Cannot_apply -> lbl.ld_type
                               in
                               (Ident.name id, ty))
                      in
                      Lwt.return
                        (Some
                           (Record_value.Record
                              { conn; env; rv; pos = 0; labels; unboxed }))
                in
                Lwt.return
                  (Some
                     (Variant
                        {
                          name = Ident.name constr.cd_id;
                          payload;
                          embed = true;
                        }))
            | exception Datarepr.Constr_not_found -> Lwt.return None )
        | _ -> Lwt.return None )
    | _ -> Lwt.return None

  let num_indexed v =
    ignore v;
    0

  let get_indexed v index =
    ignore v;
    ignore index;
    [%lwt assert false]

  let num_named v =
    let[@warning "-8"] (Variant { payload; embed; _ }) = (v [@warning "+8"]) in
    if embed then
      match payload with
      | Some payload -> Value_basic.num_named payload
      | None -> 0
    else 1

  let list_named v =
    let[@warning "-8"] (Variant { payload; embed; _ }) = (v [@warning "+8"]) in
    match payload with
    | Some payload ->
        if embed then Value_basic.list_named payload
        else Lwt.return [ ("‹1›", payload) ]
    | None -> Lwt.return []

  let is_indexed_container = false
end
