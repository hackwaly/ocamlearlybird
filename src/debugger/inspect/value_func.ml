open Value_basic

class func_value ?pc ?loc () =
  let () = ignore pc in
  object
    inherit value

    method to_short_string = "«fun»"

    method! vscode_menu_context = Some "ocamlearlybird.function"

    method! closure_code_location = loc

    method! num_named = 1

    method! list_named =
      Lwt.return
        [
          ( "‹tips›",
            new tips_value
              [| "You can use context menu to goto closure code location." |] );
        ]
  end

let adopter scene typenv obj typ =
  ignore typenv;
  match Types.get_desc typ with
  | Types.Tarrow _ ->
      let%lwt pc, loc =
        if Scene.is_block obj then
          let%lwt pc, loc = Scene.get_closure_code scene obj in
          Lwt.return (Some pc, loc)
        else Lwt.return (None, None)
      in
      Lwt.return (Some (new func_value ?pc ?loc ()))
  | _ -> Lwt.return None
