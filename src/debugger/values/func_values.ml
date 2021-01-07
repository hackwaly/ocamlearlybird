open Value_basic

module Function_value = struct
  type t += Function of Event.t

  let extension_constructor =
    Obj.Extension_constructor.of_val (Function (Obj.magic ()))

  let is_named_container = false

  let is_indexed_container = false

  let adopt conn env ty rv =
    ignore env;
    match (Ctype.repr ty).desc with
    | Types.Tarrow _ ->
        let%lwt pc = Debugcom.get_closure_code conn rv in
        let event = Symbols.find_event conn#symbols pc in
        Lwt.return (Some (Function event))
    | _ -> Lwt.return None

  let to_short_string ?(hex = false) v =
    ignore hex;
    let[@warning "-8"] (Function event) = (v [@warning "+8"]) in
    let _, line, col =
      event.ev.ev_loc.Location.loc_start |> Location.get_pos_info
    in
    let fname = event.module_.source |> Option.value ~default:"(none)" in
    Printf.sprintf "«fun» @ %s:%d:%d" (Filename.basename fname) line col

  let num_indexed v =
    ignore v;
    0

  let get_indexed v index =
    ignore v;
    ignore index;
    [%lwt assert false]

  let num_named _ = 0

  let list_named v =
    ignore v;
    Lwt.return []
end
