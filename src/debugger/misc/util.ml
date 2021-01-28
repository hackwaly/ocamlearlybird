(**
 * Copyright (C) 2021 Yuxiang Wen
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Ground

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
  let resolve module_id search_dirs =
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
      |> List.of_seq |> Lwt.return
    in
    let%lwt source_paths = derive_source_paths module_id search_dirs in
    match%lwt source_paths |> Lwt_list.find_s Lwt_unix.file_exists with
    | source -> Lwt.return (Some source)
    | exception Not_found -> Lwt.return None

  let default module_id search_dirs = resolve module_id search_dirs

  let make ?(source_dirs = []) () =
    let resolver module_id search_dirs =
      let search_dirs = search_dirs @ source_dirs |> List.uniq in
      resolve module_id search_dirs
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
