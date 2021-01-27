open Value_basic

class array_value ~scene ~typenv ~obj ~element_type ~len () =
  object
    inherit value

    method to_short_string = if len = 0 then "[||]" else "[|…|]"

    method! num_indexed = len

    method! num_named = 1

    method! list_named =
      Lwt.return [ ("‹length›", new Value_simple.int_value len) ]

    method! get_indexed i =
      let%lwt obj' = Scene.get_field scene obj i in
      adopt scene typenv obj' element_type
  end

let adopter scene typenv obj typ =
  match (Ctype.repr typ).desc with
  | Tconstr (_, [ element_type ], _)
    when Typenv.type_matches typenv (Predef.type_array element_type) typ
         && Scene.is_block obj ->
      let%lwt len = Scene.get_size scene obj in
      Lwt.return
        (Some (new array_value ~scene ~typenv ~obj ~element_type ~len ()))
  | _ -> Lwt.return None
