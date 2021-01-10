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
    let rec aux path =
      match path with
      | Pident id -> Ident.name id
      | Pdot (p, d) -> aux p ^ "." ^ d
      | Papply (p1, p2) -> aux p1 ^ " (" ^ aux p2 ^ ")"
    in
    aux path

  let rec to_longident path =
    match path with
    | Pident id -> Longident.Lident (Ident.name id)
    | Pdot (p, d) -> Longident.Ldot (to_longident p, d)
    | Papply (p1, p2) -> Longident.Lapply (to_longident p1, to_longident p2)
end

module Env = struct
  include Env

  let list_value_pos env path =
    let longid = Path.to_longident path in
    let mod_names =
      env
      |> Env.extract_modules (Some longid)
      |> List.filter (fun name ->
             env |> Env.is_structure_module (Path.Pdot (path, name)))
      |> List.map (fun name -> (`Module, name))
    in
    let val_names =
      env
      |> Env.extract_values (Some longid)
      |> List.map (fun name -> (`Value, name))
    in
    let names = mod_names @ val_names in
    ( names
      |> List.filter_map (fun (kind, name) ->
             try
               let path = Path.Pdot (path, name) in
               let addr =
                 match kind with
                 | `Value -> env |> Env.find_value_address path
                 | `Module -> env |> Env.find_module_address path
               in
               let pos =
                 match addr with Adot (_, pos) -> pos | _ -> raise Not_found
               in
               Some (kind, name, pos)
             with Not_found -> None),
      env )
end

let parse_impl =
  Lwt_util.memo ~cap:64 (fun _rec path ->
      let%lwt source, _ = Lwt_util.file_content_and_bols path in
      Lwt_preemptive.detach
        (fun source ->
          let lexbuf = Lexing.from_string source in
          Parse.implementation lexbuf)
        source)
