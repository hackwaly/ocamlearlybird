open Util

type event = { frag : int; ev : Instruct.debug_event; env : Env.t Lwt.t Lazy.t }

type module_ = {
  frag : int;
  id : string;
  mutable source : string option;
  events : event array;
}

let derive_source_paths id dirs =
  dirs |> List.to_seq
  |> Seq.flat_map (fun dir ->
         List.to_seq
           [
             dir ^ "/" ^ String.uncapitalize_ascii id ^ ".ml";
             dir ^ "/" ^ String.uncapitalize_ascii id ^ ".re";
             dir ^ "/" ^ id ^ ".ml";
             dir ^ "/" ^ id ^ ".re";
           ])
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

let load_env ev load_path () =
  try%lwt
    Lwt_mutex.with_lock load_env_mutex
      (Lwt_preemptive.detach (fun () ->
           if Load_path.get_paths () <> load_path then (
             Load_path.init load_path;
             Envaux.reset_cache () );
           Envaux.env_from_summary ev.Instruct.ev_typenv ev.ev_typsubst))
  with exc ->
    Log.warn (fun m -> m "load_env fail: %s" (Printexc.to_string exc));%lwt
    Lwt.fail exc

let resolve_source id load_path () =
  let%lwt source_paths = derive_source_paths id load_path in
  source_paths |> Lwt_list.find_s Lwt_unix.file_exists

let load frag file =
  let read_eventlists toc ic =
    let pos = seek_section toc "DBUG" in
    Lwt_io.set_position ic pos;%lwt
    let%lwt num_eventlists = Lwt_io.BE.read_int ic in
    let eventlists = ref [] in
    for%lwt i = 1 to num_eventlists do
      let%lwt orig = Lwt_io.BE.read_int ic in
      let%lwt evl = Lwt_io.read_value ic in
      let evl = (evl : Instruct.debug_event list) in
      List.iter (relocate_event orig) evl;
      let%lwt (load_path : string list) = Lwt_io.read_value ic in
      eventlists := (evl, load_path) :: !eventlists;
      Lwt.return ()
    done;%lwt
    Lwt.return (List.rev !eventlists)
  in
  let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.input file in
  (let%lwt toc = read_toc ic in
   let%lwt eventlists = read_eventlists toc ic in
   let source_by_id = Hashtbl.create 0 in
   let modules =
     eventlists |> CCList.to_iter
     |> Iter.flat_map (fun (evl, load_path) ->
            CCList.to_iter (partition_modules evl)
            |> Iter.map (fun evl ->
                   let id = (List.hd evl).Instruct.ev_module in
                   let events =
                     evl |> CCList.to_iter
                     |> Iter.map (fun ev ->
                            {
                              frag;
                              ev;
                              env = Lazy.from_fun (load_env ev load_path);
                            })
                     |> Iter.to_array
                   in
                   let cnum_of event =
                     let pos = Debug_event.lexing_position event.ev in
                     pos.pos_cnum
                   in
                   Hashtbl.replace source_by_id id
                     (Lazy.from_fun (resolve_source id load_path));
                   events |> Array.fast_sort (Compare.by cnum_of);
                   { frag; id; source = None; events }))
     |> Iter.to_list
   in
   modules
   |> Lwt_list.iter_s (fun module_ ->
          let%lwt source =
            match Hashtbl.find_opt source_by_id module_.id with
            | None -> Lwt.return None
            | Some source ->
                let%lwt source = Lazy.force source in
                Lwt.return (Some source)
          in
          module_.source <- source;
          Lwt.return ());%lwt
   Lwt.return modules)
    [%finally Lwt_io.close ic]
