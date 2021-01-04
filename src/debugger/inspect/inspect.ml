open Inspect_types
open Debuginfo

type t = {
  find_event : Pc.t -> Event.t;
  find_module : string -> Module.t;
  lock_conn : 'a. (Debugcom.conn -> 'a Lwt.t) -> 'a Lwt.t;
  alloc_obj_id : unit -> int;
  mutable scene : scene option;
}

let find_obj t id =
  match t.scene with
  | None -> raise Not_found
  | Some scene -> Hashtbl.find scene.obj_tbl id

let undef_lazy = Lazy.from_fun (fun () -> assert false)

let rec make_obj t ~name ~value ?(structured = false)
    ?(list_members = fun _ -> Lwt.return_nil) () =
  match t.scene with
  | Some scene ->
      let id = t.alloc_obj_id () in
      let obj = { id; name; value; structured; members = undef_lazy } in
      obj.members <- Lazy.from_fun (fun () -> list_members obj);
      Hashtbl.replace scene.obj_tbl id obj;
      obj
  | None -> assert false

and list_scope_obj t obj =
  let[@warning "-8"] (Scope { scene_id; index; kind; _ }) =
    (obj.value [@warning "+8"])
  in
  match t.scene with
  | Some scene when scene.report.rep_event_count = scene_id ->
      t.lock_conn (fun conn ->
          let frame = scene.frames.(index) in
          let event = frame.event in
          match%lwt Lazy.force frame.event.env with
          | exception _ ->
              Log.debug (fun m -> m "no env");%lwt
              Lwt.return []
          | env ->
              Log.debug (fun m -> m "has env");%lwt
              let ident_tbl =
                match kind with
                | `Stack -> event.ev.ev_compenv.ce_stack
                | `Heap -> event.ev.ev_compenv.ce_heap
                | `Global -> assert false
              in
              let iter_bindings f =
                Ident.iter (fun id index -> f (id, index)) ident_tbl
              in
              let find_ty env path =
                let val_desc = env |> Env.find_value path in
                let ty = Ctype.correct_levels val_desc.Types.val_type in
                ty
              in
              let to_obj (ident, pos) =
                Log.debug (fun m -> m "local_to_obj: %s" (Ident.name ident));%lwt
                match find_ty env (Path.Pident ident) with
                | exception Not_found -> Lwt.return None
                | ty ->
                    let%lwt rv =
                      match kind with
                      | `Stack ->
                          Debugcom.get_local conn (event.ev.ev_stacksize - pos)
                      | `Heap -> Debugcom.get_environment conn pos
                      | `Global -> [%lwt assert false]
                    in
                    let%lwt value = get_value t conn env rv ty in
                    let obj = make_obj t ~name:(Ident.name ident) ~value () in
                    Lwt.return (Some obj)
              in
              let%lwt objs =
                Debugcom.exec_with_frame conn index (fun _ ->
                    iter_bindings |> Iter.to_list
                    |> Lwt_list.filter_map_s to_obj)
              in
              Log.debug (fun m -> m "list_scope_obj 2");%lwt
              Lwt.return objs)
  | _ -> [%lwt assert false]

and get_value t conn env rv ty =
  try%lwt
    if Ctype.matches env Predef.type_int ty then
      let%lwt mv = Debugcom.marshal_obj conn rv in
      Lwt.return (Int (Obj.magic mv))
    else if Ctype.matches env Predef.type_float ty then
      let%lwt mv = Debugcom.marshal_obj conn rv in
      Lwt.return (Double (Obj.magic mv))
    else if Ctype.matches env Predef.type_bool ty then
      let%lwt mv = Debugcom.marshal_obj conn rv in
      Lwt.return (Bool (Obj.magic mv))
    else if Ctype.matches env Predef.type_char ty then
      let%lwt mv = Debugcom.marshal_obj conn rv in
      Lwt.return (Char (Obj.magic mv))
    else if Ctype.matches env Predef.type_string ty then
      let%lwt mv = Debugcom.marshal_obj conn rv in
      Lwt.return (String (Obj.magic mv))
    else
      match (Ctype.repr ty).desc with
      | Types.Tarrow _ ->
          let%lwt pc = Debugcom.get_closure_code conn rv in
          Log.debug (fun m -> m "%s" (Pc.show pc));%lwt
          let event = t.find_event pc in
          Lwt.return (Function { location = event.ev.ev_loc })
      | Tconstr (path, ty_args, _) -> (
          match env |> Env.find_type path with _ -> Lwt.return Unknown )
      | _ -> Lwt.return Unknown
  with ex ->
    Log.debug (fun m -> m "Inspect.get_value error %s" (Printexc.to_string ex));%lwt
    Lwt.return Unknown

let clear_scene t = t.scene <- None

let update_scene t conn report =
  let obj_tbl = Hashtbl.create 0 in
  let%lwt frames =
    match report.Debugcom.rep_type with
    | Event | Breakpoint ->
        let%lwt curr_fr_sp, _ = Debugcom.get_frame conn in
        let make_frame index sp (pc : Pc.t) =
          let event = t.find_event pc in
          {
            Frame.index;
            stack_pos = sp;
            event;
            scopes = [];
          }
        in
        let rec walk_up index stacksize frames =
          let index = index + 1 in
          match%lwt Debugcom.up_frame conn stacksize with
          | Some (sp, pc) ->
              let frame = make_frame index sp pc in
              walk_up index (Frame.stacksize frame) (frame :: frames)
          | None -> Lwt.return frames
        in
        (let%lwt sp, pc = Debugcom.initial_frame conn in
         let intial_frame = make_frame 0 sp pc in
         let%lwt frames =
           walk_up 0 (Frame.stacksize intial_frame) [ intial_frame ]
         in
         let frames = List.rev frames |> Array.of_list in
         Lwt.return frames)
          [%finally Debugcom.set_frame conn curr_fr_sp]
    | _ -> Lwt.return [||]
  in
  let scene = { report; frames; obj_tbl } in
  t.scene <- Some scene;
  (* We delay set Frame's `scopes` field. Because make_obj need agent's `scene` field to be set *)
  scene.frames
  |> Array.iter (fun (frame : Frame.t) ->
         let make_scope_obj name kind =
           make_obj t ~name
             ~value:
               (Scope
                  {
                    scene_id = report.rep_event_count;
                    index = frame.index;
                    kind;
                  })
             ~structured:true () ~list_members:(list_scope_obj t)
         in
         let scopes =
           [ make_scope_obj "Stack" `Stack; make_scope_obj "Heap" `Heap ]
         in
         frame.scopes <- scopes);
  Lwt.return ()
