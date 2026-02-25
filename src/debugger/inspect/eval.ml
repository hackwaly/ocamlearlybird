open Ground

[%%if ocaml_version >= (5, 2, 0) || (ocaml_version >= (4, 14, 3) && ocaml_version < (5, 0, 0))]
let ident_find_same_heap id (compenv: Instruct.compilation_env) =
  match compenv.ce_closure with
  | Not_in_closure -> raise Not_found
  | In_closure { entries; env_pos } ->
    match Ident.find_same id entries with
    | Free_variable pos ->
      pos - env_pos
    | Function _pos ->
      (* Recursive functions seem to be unhandled *)
      raise Not_found
[%%else]
let ident_find_same_heap id (compenv: Instruct.compilation_env) =
  Ident.find_same id compenv.ce_heap
[%%endif]

let value_path scene frame path =
  let rec address scene frame path =
    let event = frame.Frame.event |> Option.get in
    function
    | Env.Aident id -> (
        if Ident.global id then
          let globals = Lazy.force frame.globals in
          let pos = Ident.Map.find id globals in
          Scene.get_global scene pos
        else
          try%lwt
            let pos = Ident.find_same id event.ev_compenv.ce_stack in
            Scene.get_local scene frame (event.ev_stacksize - pos) (* TODO: Why subtracting from ev_stacksize? Not done in Value_scope. get_local already does that. *)
          with Not_found ->
            let pos = ident_find_same_heap id event.ev_compenv in
            Scene.get_environment scene frame pos)
    | Env.Adot (root, pos) ->
        let%lwt v = address scene frame path root in
        assert (Scene.is_block v);
        Scene.get_field scene v pos
  in
  let typenv = Lazy.force frame.Frame.typenv in
  let addr = typenv |> Typenv.find_value_address path in
  address scene frame path addr
