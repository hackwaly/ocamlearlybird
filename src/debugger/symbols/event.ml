type t = Debuginfo.event = {
  module_ : Debuginfo.module_;
  ev : Instruct.debug_event;
  env : Env.t Lwt.t Lazy.t;
}

let pc ev = { Pc.frag = ev.module_.frag; pos = ev.ev.ev_pos }
