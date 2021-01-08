module String_set = CCSet.Make (CCString)

type type_expr = Types.type_expr
let pp_type_expr fmt _t = Format.pp_print_string fmt "<opaque>"

type debug_event = Instruct.debug_event =
  { mutable ev_pos: int;
    ev_module: string;
    ev_loc: Location.t;  [@opaque]
    ev_kind: debug_event_kind;
    ev_defname: string;
    ev_info: debug_event_info;
    ev_typenv: Env.summary;  [@opaque]
    ev_typsubst: Subst.t;  [@opaque]
    ev_compenv: Instruct.compilation_env;  [@opaque]
    ev_stacksize: int;
    ev_repr: debug_event_repr }
[@@deriving show]

and debug_event_kind = Instruct.debug_event_kind =
    Event_before
  | Event_after of type_expr
  | Event_pseudo
[@@deriving show]

and debug_event_info = Instruct.debug_event_info =
    Event_function
  | Event_return of int
  | Event_other
[@@deriving show]

and debug_event_repr = Instruct.debug_event_repr =
    Event_none
  | Event_parent of int ref
  | Event_child of int ref
[@@deriving show]

type module_ = {
  frag : int;
  id : string;
  source : string option;
  mutable events : event array; [@opaque]
}
[@@deriving show]

and event = {
  module_ : module_;
  ev : debug_event;
  env : Env.t Lwt.t Lazy.t; [@opaque]
}
[@@deriving show]

let derive_source_ids id =
  let id' = Str.split (Str.regexp "__") id |> List.rev |> List.hd in
  if id' <> id then [ id; id' ] else [ id ]

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

let read_toc ic =
  let%lwt len = Lwt_io.length ic in
  let pos_trailer = Int64.sub len (Int64.of_int 16) in
  Lwt_io.set_position ic pos_trailer;%lwt
  let%lwt num_sections = Lwt_io.BE.read_int ic in
  let%lwt magic =
    Lwt_util.read_to_string_exactly ic (String.length Config.exec_magic_number)
  in
  if%lwt Lwt.return (magic <> Config.exec_magic_number) then
    Lwt.fail_invalid_arg "Bad magic";%lwt
  let pos_toc = Int64.sub pos_trailer (Int64.of_int (8 * num_sections)) in
  Lwt_io.set_position ic pos_toc;%lwt
  let section_table = ref [] in
  for%lwt i = 1 to num_sections do
    let%lwt name = Lwt_util.read_to_string_exactly ic 4 in
    let%lwt len = Lwt_io.BE.read_int ic in
    section_table := (name, len) :: !section_table;
    Lwt.return_unit
  done;%lwt
  Lwt.return (pos_toc, !section_table)

let seek_section (pos, section_table) name =
  let rec seek_sec pos = function
    | [] -> raise Not_found
    | (name', len) :: rest ->
        let pos = Int64.sub pos (Int64.of_int len) in
        if name' = name then pos else seek_sec pos rest
  in
  seek_sec pos section_table

let relocate_event orig ev =
  ev.Instruct.ev_pos <- orig + ev.Instruct.ev_pos;
  match ev.ev_repr with Event_parent repr -> repr := ev.ev_pos | _ -> ()

(* NOTE: This function relies on order of evl *)
let partition_modules evl =
  let rec partition_modules' ev evl =
    match evl with
    | [] -> ([ ev ], [])
    | ev' :: evl ->
        let evl, evll = partition_modules' ev' evl in
        if ev.Instruct.ev_module = ev'.ev_module then (ev :: evl, evll)
        else ([ ev ], evl :: evll)
  in
  match evl with
  | [] -> []
  | ev :: evl ->
      let evl, evll = partition_modules' ev evl in
      evl :: evll

let load_env_mutex = Lwt_mutex.create ()

let resolve_source id dirs () =
  let%lwt source_paths = derive_source_paths id dirs in
  source_paths |> Lwt_list.find_s Lwt_unix.file_exists

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

let load frag file =
  let read_eventlists ic toc =
    let pos = seek_section toc "DBUG" in
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
   let all_dirs = ref String_set.empty in
   let load_env ev () =
     try%lwt
       Lwt_mutex.with_lock load_env_mutex
         (Lwt_preemptive.detach (fun () ->
              let dirs = String_set.to_list !all_dirs in
              if Load_path.get_paths () <> dirs then (
                Load_path.init dirs;
                Envaux.reset_cache () );
              Envaux.env_from_summary ev.Instruct.ev_typenv ev.ev_typsubst))
     with Envaux.Error (Envaux.Module_not_found path) as exc ->
       Path.print Format.str_formatter path;
       let path_str = Format.flush_str_formatter () in
       Log.warn (fun m -> m "load_env fail: Module_not_found path %s" path_str);%lwt
       raise exc
   in
   let%lwt modules =
     eventlists |> CCList.to_iter
     |> Iter.flat_map (fun (evl, dirs) ->
            CCList.to_iter (partition_modules evl)
            |> Iter.map (fun evl -> (evl, dirs)))
     |> Iter.to_list
     |> Lwt_list.map_s (fun (evl, dirs) ->
            all_dirs := String_set.add_iter !all_dirs (CCList.to_iter dirs);
            let ev = List.hd evl in
            let id = ev.Instruct.ev_module in
            let%lwt source =
              match%lwt resolve_source id dirs () with
              | r -> Lwt.return (Some r)
              | exception _ ->
                  Log.warn (fun m -> m "Module %s source not found" id);%lwt
                  Lwt.return None
            in
            let module_ = { frag; id; source; events = [||] } in
            let events =
              evl |> CCList.to_iter
              |> Iter.map (fun ev ->
                     { module_; ev; env = Lazy.from_fun (load_env ev) })
              |> Iter.to_array
            in
            let cnum_of event =
              let pos = Util.Debug_event.lexing_position event.ev in
              pos.pos_cnum
            in
            events |> Array.fast_sort (Compare.by cnum_of);
            module_.events <- events;
            Lwt.return module_)
   in
   Lwt.return (modules, globals))
    [%finally Lwt_io.close ic]
