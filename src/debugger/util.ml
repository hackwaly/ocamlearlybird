module Debug_event = struct
  type t = Instruct.debug_event

  let lexing_position ev =
    match ev.Instruct.ev_kind with
    | Event_before -> ev.ev_loc.Location.loc_start
    | Event_after _ -> ev.ev_loc.Location.loc_end
    | Event_pseudo -> (
        match ev.Instruct.ev_info with
        | Event_return _ -> ev.ev_loc.Location.loc_end
        | _ -> ev.ev_loc.Location.loc_start )

  let is_pseudo ev =
    match ev.Instruct.ev_kind with Event_pseudo -> true | _ -> false
end

module Path = struct
  include Path

  let to_string path =
    Path.print Format.str_formatter path;
    Format.flush_str_formatter ()

  let rec to_longident path =
    match path with
    | Pident id -> Longident.Lident (Ident.name id)
    | Pdot (p, d) -> Longident.Ldot (to_longident p, d)
    | Papply (p1, p2) -> Longident.Lapply (to_longident p1, to_longident p2)
end

module Env = struct
  include Env

  let dummy_module_id = Ident.create_persistent "Temp_ywwofnzftu"

  let list_value_pos modtype =
    let mid = dummy_module_id in
    let env = Env.empty |> Env.add_module mid Types.Mp_present modtype in
    let mod_names =
      env
      |> Env.extract_modules (Some (Longident.Lident (Ident.name mid)))
      |> List.map (fun name -> (`Module, name))
    in
    let val_names =
      env
      |> Env.extract_values (Some (Longident.Lident (Ident.name mid)))
      |> List.map (fun name -> (`Value, name))
    in
    let names = mod_names @ val_names in
    ( names
      |> List.filter_map (fun (kind, name) ->
             try
               let path = Path.Pdot (Path.Pident mid, name) in
               let addr =
                 match kind with
                 | `Value -> env |> Env.find_value_address path
                 | `Module -> env |> Env.find_module_address path
               in
               let pos =
                 match addr with
                 | Adot (Aident id, pos) when Ident.same id mid -> pos
                 | _ -> raise Not_found
               in
               Some (kind, name, pos)
             with Not_found -> None),
      env )
end
