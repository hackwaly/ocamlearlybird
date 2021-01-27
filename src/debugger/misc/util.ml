open Ground
open Path_glob

module Debug_event = struct
  open Instruct

  type t = debug_event

  let lex_pos t =
    match t.ev_kind with
    | Event_after _ -> t.ev_loc.loc_end
    | _ -> t.ev_loc.loc_start

  let cnum t = (lex_pos t).pos_cnum

  let line_column t = lex_pos t |> Lexing.Position.line_column
end

module Source_resolver = struct
  let resolve ?(filter = fun _ -> true) module_id search_dirs =
    let derive_source_ids id =
      let id' = Str.split (Str.regexp "__") id |> List.rev |> List.hd in
      if id' <> id then [ id; id' ] else [ id ]
    in
    let derive_source_paths id dirs =
      let ids = derive_source_ids id in
      ids |> List.to_seq
      |> Seq.flat_map (fun id ->
             dirs |> List.to_seq
             |> Seq.flat_map (fun dir ->
                    List.to_seq
                      [
                        dir ^ "/" ^ String.uncapitalize_ascii id ^ ".ml";
                        dir ^ "/" ^ String.uncapitalize_ascii id ^ ".re";
                        dir ^ "/" ^ id ^ ".ml";
                        dir ^ "/" ^ id ^ ".re";
                      ]))
      |> Seq.filter filter |> List.of_seq |> Lwt.return
    in
    let%lwt source_paths = derive_source_paths module_id search_dirs in
    match%lwt source_paths |> Lwt_list.find_s Lwt_unix.file_exists with
    | source -> Lwt.return (Some source)
    | exception Not_found -> Lwt.return None

  let default module_id search_dirs = resolve module_id search_dirs

  let make ?(source_dirs = []) ?only_debug_glob () =
    let filter source =
      match only_debug_glob with
      | None -> true
      | Some glob -> Glob.eval glob source
    in
    let resolver module_id search_dirs =
      let search_dirs = (search_dirs @ source_dirs) |> List.uniq in
      resolve ~filter module_id search_dirs
    in
    resolver
end

module Path = struct
  open Path

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
