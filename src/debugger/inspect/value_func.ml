open Value_basic

class func_value ~pc ?event () =
  let () =
    ignore pc;
    ignore event
  in
  object
    inherit value

    method to_short_string = "«fun»"
  end

let adopter scene typenv obj typ =
  ignore typenv;
  match (Ctype.repr typ).desc with
  | Types.Tarrow _ ->
      let%lwt pc, event = Scene.get_closure_code scene obj in
      Lwt.return (Some (new func_value ~pc ?event ()))
  | _ -> Lwt.return None
