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

type t = {
  get_source_dir : string -> string option;
  debug_filter : string -> bool;
  mutable frags : Code_fragment.t Map.Make(Int).t;
  mutable source_modules : Code_module.t Map.Make(String).t;
  mutable version : int;
  dummy : unit;
}

module IntMap_ = Map.Make (Int)
module StringMap_ = Map.Make (String)

let create ?(get_source_dir = fun _ -> None) ?(debug_filter = fun _ -> true) ()
    =
  {
    get_source_dir;
    debug_filter;
    frags = IntMap_.empty;
    source_modules = StringMap_.empty;
    version = 0;
    dummy = ();
  }

let dup t = { t with dummy = () }

let add_fragment t frag =
  let resolve_module_source module_id search_dirs =
    Log.debug (fun m -> m "%s %s" module_id ([%show: string list] search_dirs));
    let search_dirs =
      match t.get_source_dir module_id with
      | Some dir -> [ dir ]
      | None -> search_dirs
    in
    let module_id' =
      Str.split (Str.regexp "__") module_id |> List.rev |> List.hd
    in
    let source_paths =
      search_dirs |> List.to_seq
      |> Seq.flat_map (fun dir ->
             List.to_seq
               [
                 dir ^ "/" ^ String.uncapitalize_ascii module_id' ^ ".ml";
                 dir ^ "/" ^ String.uncapitalize_ascii module_id' ^ ".re";
                 dir ^ "/" ^ module_id' ^ ".ml";
                 dir ^ "/" ^ module_id' ^ ".re";
               ])
      |> List.of_seq
    in
    source_paths |> Lwt_list.find_s Lwt_unix.file_exists
  in
  let resolve_module (module_ : Code_module.t) =
    match%lwt resolve_module_source module_.module_id module_.search_dirs with
    | source_path ->
        t.source_modules <-
          t.source_modules |> StringMap_.add source_path module_;
        let%lwt source = Source.from_path source_path in
        module_.source <- Some source;
        Lwt.return ()
    | exception Not_found -> Lwt.return ()
  in
  let resolve_fragment frag =
    frag |> Code_fragment.to_modules_seq |> Lwt_seq.iter_s resolve_module
  in
  resolve_fragment frag;%lwt
  t.frags <- t.frags |> IntMap_.add frag.num frag;
  t.version <- t.version + 1;
  Lwt.return ()

let remove_fragment t frag_num =
  t.source_modules <-
    t.source_modules
    |> StringMap_.filter (fun _ (module_ : Code_module.t) ->
           module_.frag <> frag_num);
  t.frags <- t.frags |> IntMap_.remove frag_num;
  t.version <- t.version + 1

let find_fragment t frag_num = t.frags |> IntMap_.find frag_num

let find_event t (frag_num, code_pos) =
  let frag = find_fragment t frag_num in
  Hashtbl.find frag.event_tbl code_pos

let find_event_opt t pc = try Some (find_event t pc) with Not_found -> None

let to_fragments_seq t = t.frags |> IntMap_.to_seq |> Seq.map snd

let find_source_module source t = t.source_modules |> StringMap_.find source

let to_source_modules_seq t =
  t.source_modules |> StringMap_.to_seq |> Seq.map snd
