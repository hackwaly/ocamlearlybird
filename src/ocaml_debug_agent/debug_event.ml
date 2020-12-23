type t = Instruct.debug_event

let lexing_position ev =
  match ev.Instruct.ev_kind with
  | Event_before -> ev.ev_loc.Location.loc_start
  | Event_after _ -> ev.ev_loc.Location.loc_end
  | _ -> ev.ev_loc.Location.loc_start

let is_pseudo ev =
  match ev.Instruct.ev_kind with
  | Event_pseudo -> true
  | _ -> false

let cnum_of ev = (lexing_position ev).Lexing.pos_cnum
