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
module StringSet_ = Set.Make (String)

type t = {
  num : int;
  module_tbl : (string, Code_module.t) Hashtbl.t;
  event_tbl : (int, Instruct.debug_event) Hashtbl.t;
  globals : int Ident.Map.t;
  all_search_dirs : string list;
}

let make frag_num debug_info =
  let module_tbl = Hashtbl.create 0 in
  let event_tbl = Hashtbl.create 0 in
  let globals, evls = debug_info in
  let all_search_dirs = ref StringSet_.empty in
  let process_evl (evl, search_dirs) =
    all_search_dirs :=
      StringSet_.union !all_search_dirs (StringSet_.of_list search_dirs);
    let module_eq = Compare.by (fun ev -> ev.ev_module) |> Compare.to_equal in
    let process_module_evl evl =
      let module_id = (List.hd evl).ev_module in
      let events =
        evl |> List.to_seq
        |> Seq.tap (fun ev -> Hashtbl.replace event_tbl ev.ev_pos ev)
        |> Seq.filter (fun ev -> ev.ev_kind <> Event_pseudo)
        |> Array.of_seq
      in
      Array.fast_sort (Compare.by ~cmp:Int.compare Util.Debug_event.cnum) events;
      let module_ =
        {
          Code_module.frag = frag_num;
          module_id;
          search_dirs;
          source = None;
          events;
        }
      in
      Hashtbl.replace module_tbl module_id module_
    in
    evl |> List.group_consecutive module_eq |> List.iter process_module_evl
  in
  evls |> List.iter process_evl;
  {
    num = frag_num;
    module_tbl;
    event_tbl;
    globals;
    all_search_dirs = !all_search_dirs |> StringSet_.to_seq |> List.of_seq;
  }

let find_module t module_id = Hashtbl.find t.module_tbl module_id

let to_modules_seq t = t.module_tbl |> Hashtbl.to_seq_values

let to_source_modules_seq t =
  t |> to_modules_seq
  |> Seq.filter (fun (it : Code_module.t) -> it.source |> Option.is_some)
