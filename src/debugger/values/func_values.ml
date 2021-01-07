open Value_basic
open Misc_values

module Function_value = struct
  type t += Function of Event.t option

  let extension_constructor =
    Obj.Extension_constructor.of_val (Function (Obj.magic ()))

  let is_named_container = true

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

  let num_named _ = 1

  let list_named v =
    let[@warning "-8"] (Function event) = (v [@warning "+8"]) in
    let pos =
      match event with
      | Some event ->
          let _, line, col =
            event.ev.ev_loc.Location.loc_start |> Location.get_pos_info
          in
          let fname = event.module_.source |> Option.value ~default:"(none)" in
          Printf.sprintf "%s:%d:%d" fname line col
      | None -> "«no debug info»"
    in
    Lwt.return [ (Ident.create_local "·defined_at", Raw_string_value.Raw_string pos) ]
end
