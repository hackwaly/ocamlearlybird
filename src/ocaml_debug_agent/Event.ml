type t = Debug_info.event = {
  frag : int;
  ev : Instruct.debug_event;
  env : Env.t Lwt.t Lazy.t;
}

let pc ev = { Pc.frag = ev.frag; pos = ev.ev.ev_pos }
