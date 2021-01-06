module Debug_event = struct
  type t = Instruct.debug_event

  let lexing_position ev =
    match ev.Instruct.ev_kind with
    | Event_before -> ev.ev_loc.Location.loc_start
    | Event_after _ -> ev.ev_loc.Location.loc_end
    | Event_pseudo ->
      match ev.Instruct.ev_info with
      | Event_return _ -> ev.ev_loc.Location.loc_end
      | _ -> ev.ev_loc.Location.loc_start

  let is_pseudo ev =
    match ev.Instruct.ev_kind with Event_pseudo -> true | _ -> false
end
