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
open Instruct

type debug_info = (Instruct.debug_event list * string list) list

let load_debuginfo file =
  let read_toc ic =
    let%lwt len = Lwt_io.length ic in
    let pos_trailer = Int64.sub len (Int64.of_int 16) in
    Lwt_io.set_position ic pos_trailer;%lwt
    let%lwt num_sections = Lwt_io.BE.read_int ic in
    let%lwt magic =
      Lwt_io.read_string_exactly ic (String.length Config.exec_magic_number)
    in
    if%lwt Lwt.return (magic <> Config.exec_magic_number) then
      Lwt.fail_invalid_arg "Bad magic";%lwt
    let pos_toc = Int64.sub pos_trailer (Int64.of_int (8 * num_sections)) in
    Lwt_io.set_position ic pos_toc;%lwt
    let section_table = ref [] in
    for%lwt i = 1 to num_sections do
      let%lwt name = Lwt_io.read_string_exactly ic 4 in
      let%lwt len = Lwt_io.BE.read_int ic in
      section_table := (name, len) :: !section_table;
      Lwt.return_unit
    done;%lwt
    Lwt.return (pos_toc, !section_table)
  in
  let seek_section (pos, section_table) name =
    let rec seek_sec pos = function
      | [] -> raise Not_found
      | (name', len) :: rest ->
          let pos = Int64.sub pos (Int64.of_int len) in
          if name' = name then (pos, len) else seek_sec pos rest
    in
    seek_sec pos section_table
  in
  let read_global_table ic toc =
    let pos, _ = seek_section toc "SYMB" in
    Lwt_io.set_position ic pos;%lwt
    let module T = struct
      type t = { cnt : int; tbl : int Ident.Map.t }
    end in
    let%lwt (global_table : T.t) = Lwt_io.read_value ic in
    Lwt.return global_table.tbl
  in
  let relocate_event orig ev =
    ev.Instruct.ev_pos <- orig + ev.Instruct.ev_pos;
    match ev.ev_repr with Event_parent repr -> repr := ev.ev_pos | _ -> ()
  in
  let read_eventlists ic toc =
    let pos, _ = seek_section toc "DBUG" in
    Lwt_io.set_position ic pos;%lwt
    let%lwt num_eventlists = Lwt_io.BE.read_int ic in
    let eventlists = ref [] in
    for%lwt i = 1 to num_eventlists do
      let%lwt orig = Lwt_io.BE.read_int ic in
      let%lwt evl = Lwt_io.read_value ic in
      let evl = (evl : Instruct.debug_event list) in
      List.iter (relocate_event orig) evl;
      let%lwt (dirs : string list) = Lwt_io.read_value ic in
      eventlists := (evl, dirs) :: !eventlists;
      Lwt.return ()
    done;%lwt
    Lwt.return (List.rev !eventlists)
  in
  let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.input file in
  (let%lwt toc = read_toc ic in
   let%lwt globals = read_global_table ic toc in
   let%lwt eventlists = read_eventlists ic toc in
   Lwt.return (globals, eventlists))
    [%finally Lwt_io.close ic]
