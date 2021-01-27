open Value_basic

let nil_value =
  object
    inherit value

    method to_short_string = "[]"
  end

class list_value ~scene ~typenv ~obj ~element_type () =
  object
    inherit value

    method to_short_string = "‹hd› :: ‹tl›"

    method! num_named = 2

    method! list_named =
      let%lwt hd =
        let%lwt obj' = Scene.get_field scene obj 0 in
        adopt scene typenv obj' element_type
      in
      let%lwt tl =
        let%lwt obj' = Scene.get_field scene obj 1 in
        Lwt.return (new list_value ~scene ~typenv ~obj:obj' ~element_type ())
      in
      Lwt.return [ ("‹hd›", hd); ("‹tl›", tl) ]
  end

let adopter scene typenv obj typ =
  match (Ctype.repr typ).desc with
  | Tconstr (_, [ element_type ], _)
    when Typenv.type_matches typenv (Predef.type_list element_type) typ ->
      if Scene.is_block obj then
        Lwt.return (Some (new list_value ~scene ~typenv ~obj ~element_type ()))
      else Lwt.return (Some nil_value)
  | _ -> Lwt.return None
