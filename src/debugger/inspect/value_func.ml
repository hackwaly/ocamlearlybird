open Value_basic

class func_value ?event () =
  let () = ignore event in
  object
    inherit value

    method to_short_string = "«fun»"
  end

let adopter scene typenv obj typ =
  ignore typenv;
  match (Ctype.repr typ).desc with
  | Types.Tarrow _ ->
      let%lwt event =
        if Scene.is_block obj then Scene.get_closure_code scene obj
        else Lwt.return None
      in
      Lwt.return (Some (new func_value ?event ()))
  | _ -> Lwt.return None
