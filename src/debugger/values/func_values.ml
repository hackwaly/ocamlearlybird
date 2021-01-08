open Value_basic

module Function_value = struct
  include Impl_base_value

  type t += Function of Event.t option

  let extension_constructor =
    Obj.Extension_constructor.of_val (Function (Obj.magic ()))

  let adopt conn env ty rv =
    ignore env;
    match (Ctype.repr ty).desc with
    | Types.Tarrow _ ->
        let%lwt pc = Debugcom.get_closure_code conn rv in
        let event =
          match Symbols.find_event conn#symbols pc with
          | event -> Some event
          | exception Not_found -> None
        in
        Lwt.return (Some (Function event))
    | _ -> Lwt.return None

  let to_short_string ?(hex = false) _ =
    ignore hex;
    "«fun»"
end
