open Value_basic

module Function_value = struct
  type t += Function of Event.t option

  let extension_constructor =
    Obj.Extension_constructor.of_val (Function (Obj.magic ()))

  let is_indexed_container = false

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

  let to_short_string ?(hex = false) v =
    ignore hex;
    ignore v;
    "«fun»"

  let num_indexed v =
    ignore v;
    0

  let get_indexed v index =
    ignore v;
    ignore index;
    [%lwt assert false]

  let num_named _ = 0

  let list_named _ =
    Lwt.return []
end
