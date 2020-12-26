open Inspect_types

let get_value symbols conn env rv ty =
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
          Log.debug (fun m -> m "%s" (Debug_types.show_pc pc));%lwt
          let event = Symbols.find_event symbols pc in
          Lwt.return (Function { location = event.ev_loc })
      | Tconstr(path, ty_args, _) -> (
        match env |> Env.find_type path with
        | _ -> Lwt.return Unknown
      )
      | _ -> Lwt.return Unknown
  with ex ->
    Log.debug (fun m -> m "Inspect.get_value error %s" (Printexc.to_string ex));%lwt
    Lwt.return Unknown
