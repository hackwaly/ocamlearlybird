open Remote_debugger

module Module = struct
  type t = {
    frag : int;
    id : string;
    resolved_source : string option;
    events : Instruct.debug_event array;
  }

  let find_event m line column =
    let expand_to_equivalent_range code cnum =
      (* TODO: Support skip comments *)
      let is_whitespace_or_semicolon c =
        match c with ' ' | '\t' | '\r' | '\n' | ';' -> true | _ -> false
      in
      assert (cnum >= 0 && cnum < String.length code);
      let c = code.[cnum] in
      if is_whitespace_or_semicolon c then
        let rec aux f n =
          let n' = f n in
          let c = code.[n'] in
          if is_whitespace_or_semicolon c then aux f n' else Lwt.return n
        in
        let%lwt l = aux (fun x -> x - 1) cnum in
        let%lwt r = aux (fun x -> x + 1) cnum in
        Lwt.return (l, r + 1)
      else Lwt.return (cnum, cnum)
    in
    let find code events cnum =
      let%lwt l, r = expand_to_equivalent_range code cnum in
      assert (l <= r);
      let cmp ev () =
        let ev_cnum = Debug_event.cnum_of ev in
        if ev_cnum < l then -1 else if ev_cnum > r then 1 else 0
      in
      Lwt.return
        ( match events |> Array_util.bsearch ~cmp () with
        | `At i -> events.(i)
        | _ -> raise Not_found )
    in
    let%lwt code, bols =
      Lwt_util.file_content_and_bols
        ( try m.resolved_source |> Option.get
          with Invalid_argument _ -> raise Not_found )
    in
    let bol = bols.(line - 1) in
    let cnum = bol + column in
    let%lwt ev = find code m.events cnum in
    Lwt.return ev
end

type eventlist = { evl : Instruct.debug_event list; dirs : string list }

type t = {
  event_by_pc : (pc, Instruct.debug_event) Hashtbl.t;
  commit_queue : (pc, unit) Hashtbl.t;
  committed : (pc, unit) Hashtbl.t;
  module_by_id : (string, Module.t) Hashtbl.t;
  module_by_digest : (string, Module.t) Hashtbl.t;
  mutable source_dirs : string list;
  mutable version : int;
}

let derive_source_paths ~id ~dirs =
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

let create () =
  {
    event_by_pc = Hashtbl.create 0;
    commit_queue = Hashtbl.create 0;
    committed = Hashtbl.create 0;
    module_by_id = Hashtbl.create 0;
    module_by_digest = Hashtbl.create 0;
    source_dirs = [];
    version = 0;
  }

let version t = t.version

let source_dirs t = t.source_dirs

let to_seq_modules t = t.module_by_id |> Hashtbl.to_seq_values

let to_seq_events t = t.event_by_pc |> Hashtbl.to_seq_values

let commit t (module Rdbg : Remote_debugger.S) conn =
  let commit_one pc =
    let committed = Hashtbl.mem t.committed pc in
    if%lwt Lwt.return (not committed) then (
      Hashtbl.replace t.committed pc ();
      Rdbg.set_event conn pc )
  in
  Log.debug (fun m -> m "symbols commit start");%lwt
  t.commit_queue |> Hashtbl.to_seq_keys |> Lwt_util.iter_seq_s commit_one;%lwt
  Hashtbl.reset t.commit_queue;
  Log.debug (fun m -> m "symbols commit end");%lwt
  Lwt.return ()

let load t ~frag path =
  let read_toc ic =
    let%lwt len = Lwt_io.length ic in
    let pos_trailer = Int64.sub len (Int64.of_int 16) in
    Lwt_io.set_position ic pos_trailer;%lwt
    let%lwt num_sections = Lwt_io.BE.read_int ic in
    let%lwt magic =
      Lwt_util.read_to_string_exactly ic
        (String.length Config.exec_magic_number)
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
  in
  let seek_section (pos, section_table) name =
    let rec seek_sec pos = function
      | [] -> raise Not_found
      | (name', len) :: rest ->
          let pos = Int64.sub pos (Int64.of_int len) in
          if name' = name then pos else seek_sec pos rest
    in
    seek_sec pos section_table
  in
  let relocate_event orig ev =
    ev.Instruct.ev_pos <- orig + ev.Instruct.ev_pos;
    match ev.ev_repr with Event_parent repr -> repr := ev.ev_pos | _ -> ()
  in
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
  in
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
      let%lwt dirs = Lwt_io.read_value ic in
      let dirs = (dirs : string list) in
      eventlists := { evl; dirs } :: !eventlists;
      Lwt.return ()
    done;%lwt
    Lwt.return (List.rev !eventlists)
  in
  let module String_set = Set.Make (String) in
  let source_dirs = ref String_set.empty in
  let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.input path in
  (let%lwt toc = read_toc ic in
   let%lwt eventlists = read_eventlists toc ic in
   eventlists
   |> Lwt_list.iter_s (fun { evl; dirs } ->
          partition_modules evl
          |> Lwt_list.iter_s (fun evl ->
                 let id = (List.hd evl).Instruct.ev_module in
                 let%lwt source_paths = derive_source_paths ~id ~dirs in
                 let%lwt resolved_source =
                   try%lwt
                     let%lwt source_path =
                       source_paths |> Lwt_list.find_s Lwt_unix.file_exists
                     in
                     Lwt.return (Some source_path)
                   with Not_found -> Lwt.return None
                 in
                 evl
                 |> List.iter (fun ev ->
                        let pc = { frag; pos = ev.Instruct.ev_pos } in
                        Hashtbl.replace t.event_by_pc pc ev;
                        Hashtbl.replace t.commit_queue pc ());
                 let events =
                   evl
                   |> List.filter (fun ev -> not (Debug_event.is_pseudo ev))
                   |> Array.of_list
                 in
                 Array.fast_sort (Compare.by Debug_event.cnum_of) events;
                 let module_ = Module.{ frag; id; resolved_source; events } in
                 Hashtbl.replace t.module_by_id id module_;
                 ( match resolved_source with
                 | Some source ->
                     let%lwt digest = Lwt_util.digest_file source in
                     Hashtbl.replace t.module_by_digest digest module_;
                     Lwt.return ()
                 | None -> Lwt.return () );%lwt
                 Lwt.return ())))
    [%finally Lwt_io.close ic];%lwt
  t.version <- t.version + 1;
  t.source_dirs <- source_dirs.contents |> String_set.to_seq |> List.of_seq;
  Lwt.return ()

let find_module t id = Hashtbl.find t.module_by_id id

let find_module_by_source t source =
  let%lwt digest = Lwt_util.digest_file source in
  Hashtbl.find t.module_by_digest digest |> Lwt.return

let find_event t pc = Hashtbl.find t.event_by_pc pc
