open Instruct

module String_set = BatSet.Make (String)

type debug_module_info = {
  name : string;
  source : string option;
  events : debug_event array;
}

type t = {
  global_table : int Ident.Map.t;
  all_dirs : string list;
  module_info_tbl : (string, debug_module_info) Hashtbl.t;
  event_by_pc : (int, debug_event) Hashtbl.t;
}

exception Bad_magic_number

let read_exactly ic count =
  let buf = Bytes.create count in
  Lwt_io.read_into_exactly ic buf 0 count;%lwt
  Lwt.return (Bytes.to_string buf)

let read_toc ic =
  let%lwt len = Lwt_io.length ic in
  let pos_trailer = Int64.sub len (Int64.of_int 16) in
  Lwt_io.set_position ic pos_trailer;%lwt
  let%lwt num_sections = Lwt_io.BE.read_int ic in
  let%lwt magic = read_exactly ic (String.length Config.exec_magic_number) in
  if magic <> Config.exec_magic_number then raise Bad_magic_number;
  let pos_toc = Int64.sub pos_trailer (Int64.of_int (8 * num_sections)) in
  Lwt_io.set_position ic pos_toc;%lwt
  let section_table = ref [] in
  for%lwt i = 1 to num_sections do
    let%lwt name = read_exactly ic 4 in
    let%lwt len = Lwt_io.BE.read_int ic in
    section_table := (name, len) :: !section_table;
    Lwt.return_unit;
  done;%lwt
  Lwt.return (pos_toc, !section_table)

let seek_section (pos, section_table) name =
  let rec seek_sec pos = function
    | [] -> raise Not_found
    | (name', len) :: rest ->
      let pos = Int64.sub pos (Int64.of_int len) in
      if name' = name then pos
      else seek_sec pos rest
  in seek_sec pos section_table

let relocate_event orig ev =
  ev.ev_pos <- orig + ev.ev_pos;
  match ev.ev_repr with
  | Event_parent repr -> repr := ev.ev_pos
  | _ -> ()

let partition_modules evl =
  let rec partition_modules' ev evl =
    match evl with
      [] -> [ev],[]
    | ev'::evl ->
      let evl,evll = partition_modules' ev' evl in
      if ev.ev_module = ev'.ev_module then ev::evl,evll else [ev],evl::evll
  in
  match evl with
    [] -> []
  | ev::evl -> let evl,evll = partition_modules' ev evl in evl::evll

let resolve_file file dirs =
  let dir_set = ref String_set.empty in
  List.iter (fun dir ->
    let rec add_dir dir =
      if not (String_set.mem dir !dir_set) then (
        dir_set := String_set.add dir !dir_set;
        let pdir = Filename.dirname dir in
        if pdir <> "" && pdir <> dir then (
          add_dir pdir
        )
      )
    in
    add_dir dir
  ) dirs;
  let rec rec_resolve file dirs =
    match dirs with
    | dir :: dirs ->
      let path = (dir ^ "/" ^ file) in
      if%lwt Lwt_unix.file_exists path then Lwt.return_some path
      else rec_resolve file dirs
    | [] -> Lwt.return_none
  in
  rec_resolve file dirs

let pos_of_event (ev : debug_event) : Lexing.position =
  match ev.ev_kind with
  | Event_before -> ev.ev_loc.Location.loc_start
  | Event_after _ -> ev.ev_loc.Location.loc_end
  | _ -> ev.ev_loc.Location.loc_start

let read_global_table ic toc =
  let pos = seek_section toc "SYMB" in
  Lwt_io.set_position ic pos;%lwt
  let module T = struct
    type t = {
      cnt : int;
      tbl : int Ident.Map.t;
    }
  end in
  let%lwt (global_table : T.t) = Lwt_io.read_value ic in
  Lwt.return global_table.tbl

let read_symbols ?(dot_merlins=[]) ic toc =
  let%lwt global_table = read_global_table ic toc in
  let pos = seek_section toc "DBUG" in
  Lwt_io.set_position ic pos;%lwt
  let%lwt num_eventlists = Lwt_io.BE.read_int ic in
  let module_info_tbl = Hashtbl.create 0 in
  let event_by_pc = Hashtbl.create 0 in
  let all_dirs = ref String_set.empty in
  Lwt_list.iter_s (fun dot_merlin_path ->
    let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.Input dot_merlin_path in
    (
      try%lwt
        while%lwt true do
          let%lwt line = Lwt_io.read_line ic in
          if%lwt Lwt.return (BatString.starts_with line "B ") then (
            let path = line |> BatString.lchop ~n:2 |> BatString.trim in
            let path =
              if Filename.is_relative path
              then Filename.concat (Filename.dirname dot_merlin_path) path
              else path
            in
            all_dirs := String_set.add path !all_dirs;
            Lwt.return_unit
          )
        done
      with End_of_file -> Lwt.return_unit
    )[%finally Lwt_io.close ic]
  ) dot_merlins;%lwt
  for%lwt i = 1 to num_eventlists do
    let%lwt orig = Lwt_io.BE.read_int ic in
    let%lwt evl = Lwt_io.read_value ic in
    let evl = (evl : Instruct.debug_event list) in
    List.iter (relocate_event orig) evl;
    let%lwt dirs = Lwt_io.read_value ic in
    let dirs = (dirs : string list) in
    List.iter (fun dir ->
      all_dirs := String_set.add dir !all_dirs
    ) dirs;
    let evll = partition_modules evl in
    Lwt_list.iter_s (fun evl ->
      let name = (List.hd evl).ev_module in
      (* TODO: Find better way to resolve source. *)
      let fname = ref None in
      List.iter (fun ev ->
        let file = ev.ev_loc.loc_start.pos_fname in
        if file <> "_none_" then (
          fname := Some file;
        );
        Hashtbl.add event_by_pc ev.ev_pos ev
      ) evl;
      let fname = !fname |> BatOption.default_delayed (fun () -> String.uncapitalize_ascii name ^ ".ml") in
      let%lwt source = resolve_file fname dirs in
      let events = (
        evl
        |> List.filter (fun ev ->
          match ev.ev_kind with
          | Event_pseudo -> false
          | _ -> true
        )
        |> Array.of_list
      ) in
      let cmp ev1 ev2 =
        let open Lexing in
        compare
          (pos_of_event ev1).pos_cnum
          (pos_of_event ev2).pos_cnum
      in
      Array.sort cmp events;
      let module_info = {name; source; events} in
      Hashtbl.add module_info_tbl name module_info;
      Lwt.return_unit
    ) evll
  done;%lwt
  Lwt.return {
    global_table;
    event_by_pc;
    all_dirs = String_set.to_list !all_dirs;
    module_info_tbl;
  }

let load ?(dot_merlins=[]) (filename : string) : t option Lwt.t =
  let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.Input filename in
  (
    try%lwt
      let%lwt toc = read_toc ic in
      let%lwt symbols = read_symbols ~dot_merlins ic toc in
      Lwt.return_some symbols
    with Bad_magic_number | Not_found -> Lwt.return_none
  )[%finally Lwt_io.close ic]

let get_global_position symbols id =
  Ident.Map.find id symbols.global_table

let all_dirs (symbols : t) : string list =
  symbols.all_dirs

let line_column_of_pos (pos : Lexing.position) : int * int =
  Lexing.(pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)

let line_column_of_event (ev : debug_event) : int * int =
  line_column_of_pos (pos_of_event ev)

let path_to_modname (path : string) : string =
  String.capitalize_ascii Filename.(path |> basename |> remove_extension)

let get_module_info (symbols : t) (modname : string) : debug_module_info =
  Hashtbl.find symbols.module_info_tbl modname

let event_at_pc (symbols : t) (pc : int) : debug_event =
  Hashtbl.find symbols.event_by_pc pc

let module_infos (symbols : t) : debug_module_info list =
  BatHashtbl.values symbols.module_info_tbl |> BatList.of_enum

let find_event_opt_near_pos (symbols : t) (modname : string) (pos : (int * int)) : debug_event option =
  let modinfo = BatHashtbl.find_option symbols.module_info_tbl modname in
  match modinfo with
  | Some modinfo -> (
      let events = modinfo.events in
      let (line1, col1) = pos in
      let cmp ev =
        let (line2, col2) = line_column_of_event ev in
        if line1 = line2 then col1 - col2 else line1 - line2
      in
      let rec bsearch lo hi =
        if lo >= hi then hi
        else begin
          let pivot = (lo + hi) / 2 in
          let ev = events.(pivot) in
          let r = cmp ev in
          if r > 0 then bsearch (pivot + 1) hi
          else bsearch lo pivot
        end
      in
      let i = bsearch 0 (Array.length events) in
      let j = i -1 in
      if i >= Array.length events || j < 0 then
        let ev = events.(if j < 0 then i else j) in
        let cr = cmp ev in
        match ev.ev_kind with
        | Event_before when cr <= 0 -> Some ev
        | Event_after _ when cr >= 0 -> Some ev
        | _ -> None
      else
        let ev1 = events.(j) in
        let ev2 = events.(i) in
        let cr1 = cmp ev1 in
        let cr2 = cmp ev2 in
        match ev1.ev_kind, ev2.ev_kind with
        | _, Event_after _ when cr2 >= 0 -> Some ev2
        | Event_before, _ when cr1 <= 0 -> Some ev1
        | Event_after _, Event_before when cr1 >= 0 && cr2 <= 0 ->
          let p1 = pos_of_event ev1 in
          let p2 = pos_of_event ev2 in
          let l, c = line1, col1 in
          let l1, c1 = line_column_of_pos p1 in
          let l2, c2 = line_column_of_pos p2 in
          Some (
            if l1 = l2 then (
              if abs (c1 - c) < (c2 - c) then ev1 else ev2
            )
            else (
              if abs (l1 - l) < (l2 - l) then ev1 else ev2
            )
          )
        | Event_after _, _ when cr1 >= 0 -> Some ev1
        | _, Event_before when cr2 <= 0 -> Some ev2
        | _ -> None
    )
  | None ->
    None
