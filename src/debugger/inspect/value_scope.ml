open Ground
open Value_basic
open Frame

class virtual scope_value =
  object
    inherit value

    method to_short_string = "«scope»"

    method! num_named = -1
  end

[%%if ocaml_version >= (5, 2, 0) || (ocaml_version >= (4, 14, 3) && ocaml_version < (5, 0, 0))]
let iter_compenv_heap f (compenv: Instruct.compilation_env) =
  match compenv.ce_closure with
  | Not_in_closure -> ()
  | In_closure { entries; env_pos } ->
    entries
    |> Ident.iter (fun id (entry: Instruct.closure_entry) ->
        match entry with
        | Free_variable pos ->
          f (id, pos - env_pos)
        | Function _pos ->
          (* Recursive functions seem to be unhandled *)
          ()
      )
[%%else]
let iter_compenv_heap f (compenv: Instruct.compilation_env) =
  compenv.ce_heap |> Ident.iter (fun id pos -> f (id, pos))
[%%endif]

class local_scope_value ~scene ~frame ~kind () =
  let variables_and_accu_ty =
    Lazy.from_fun (fun () ->
        match frame.event with
        | None -> ([||], None)
        | Some event -> (
            let typenv = Lazy.force frame.typenv in
            let iter f =
              match kind with
              | `Stack -> event.ev_compenv.ce_stack |> Ident.iter (fun id pos -> f (id, pos))
              | `Heap -> iter_compenv_heap f event.ev_compenv
            in
            ( Iter.to_list iter
              |> List.fast_sort (Compare.by (fun (_, pos) -> pos))
              |> List.to_seq
              |> Seq.filter_map (fun (id, pos) ->
                     match typenv |> Typenv.find_value (Path.Pident id) with
                     | exception Not_found -> None
                     | { val_type; val_kind; _ } ->
                         let ty = Ctype.duplicate_type val_type in
                         Some (Ident.name id, val_kind, ty, pos))
              |> Array.of_seq,
              match (frame.index, event.ev_kind) with
              | 0, Event_after ty -> Some ty
              | _ -> None )))
  in
  object
    inherit scope_value

    method! num_named =
      let variables, accu_ty = Lazy.force variables_and_accu_ty in
      Array.length variables + if Option.is_some accu_ty then 1 else 0

    method! list_named =
      let typenv = Lazy.force frame.typenv in
      let variables, accu_ty = Lazy.force variables_and_accu_ty in
      let%lwt pairs =
        variables |> Array.to_list
        |> Lwt_list.map_s (fun (id, val_kind, ty, pos) ->
               let%lwt rv =
                 match kind with
                 | `Stack -> Scene.get_local scene frame pos
                 | `Heap -> Scene.get_environment scene frame pos
               in
               match val_kind with
               | Types.Val_ivar (_, cl_num) -> (
                   try
                     let p0, _ =
                       Typenv.find_value_by_name
                         (Longident.Lident ("self-" ^ cl_num))
                         typenv
                     in
                     let%lwt v = Eval.value_path scene frame p0 in
                     let%lwt i = Scene.marshal_obj scene rv in
                     let%lwt rv' = Scene.get_field scene v i in
                     let%lwt obj = adopt scene typenv rv' ty in
                     Lwt.return (id, obj)
                   with _ -> Lwt.return (id, uninitialized_value))
               | _ ->
                   let%lwt obj = adopt scene typenv rv ty in
                   Lwt.return (id, obj))
      in
      match accu_ty with
      | None -> Lwt.return pairs
      | Some ty ->
          let%lwt rv' = Scene.get_accu scene frame in
          let%lwt obj = adopt scene typenv rv' ty in
          Lwt.return (("%accu", obj) :: pairs)
  end

class global_scope_value ~scene ~frame () =
  object
    inherit scope_value

    method! list_named =
      let typenv = Lazy.force frame.typenv in
      Lazy.force frame.globals |> Ident.Map.to_seq
      |> Seq.filter (fun (name, _) ->
             try typenv |> Typenv.is_structure_module (Path.Pident name)
             with _ -> false)
      |> List.of_seq
      |> Lwt_list.filter_map_s (fun (id, pos) ->
             let%lwt obj = Scene.get_global scene pos in
             if Scene.is_block obj then
               Lwt.return
                 (Some
                    ( Ident.name id,
                      new Value_module.module_value
                        ~scene ~typenv ~obj ~path:(Path.Pident id) () ))
             else Lwt.return None)
  end
