open Ground
open Value_basic
open Frame

class virtual scope_value =
  object
    inherit value

    method to_short_string = "«scope»"

    method! num_named = -1
  end

class local_scope_value ~scene ~frame ~kind () =
  let variables =
    Lazy.from_fun (fun () ->
        match frame.event with
        | None -> [||]
        | Some event ->
            let typenv = Lazy.force frame.typenv in
            let compenv =
              match kind with
              | `Stack -> event.ev_compenv.ce_stack
              | `Heap -> event.ev_compenv.ce_heap
            in
            let iter f = compenv |> Ident.iter (fun id pos -> f (id, pos)) in
            Iter.to_list iter
            |> List.fast_sort (Compare.by (fun (_, pos) -> pos))
            |> List.to_seq
            |> Seq.filter_map (fun (id, pos) ->
                   match typenv |> Typenv.find_value (Path.Pident id) with
                   | exception Not_found -> None
                   | { val_type; _ } ->
                       let ty = Ctype.correct_levels val_type in
                       Some (Ident.name id, ty, pos))
            |> Array.of_seq)
  in
  object
    inherit scope_value

    method! num_named = Lazy.force variables |> Array.length

    method! list_named =
      let typenv = Lazy.force frame.typenv in
      Lazy.force variables |> Array.to_list
      |> Lwt_list.map_s (fun (id, ty, pos) ->
             let%lwt rv =
               match kind with
               | `Stack -> Scene.get_local scene frame pos
               | `Heap -> Scene.get_environment scene pos
             in
             let%lwt obj = adopt scene typenv rv ty in
             Lwt.return (id, obj))
  end

class global_scope_value ~scene ~frame () =
  object
    inherit scope_value

    method! list_named =
      let typenv = Lazy.force frame.typenv in
      Lazy.force frame.globals
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
