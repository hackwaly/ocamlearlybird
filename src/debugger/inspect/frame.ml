open Util
open Inspect_types

type t = Inspect_types.stack_frame = {
  index : int;
  stack_pos : int;
  event : Debuginfo.event;
  mutable scopes : obj list;
}

let stacksize t = t.event.ev.ev_stacksize

let defname t = t.event.ev.ev_defname

let module_ t = t.event.module_

let pc t = { Pc.frag = t.event.module_.frag; pos = t.event.ev.ev_pos }

let loc t =
  if t.index = 0 then
    let pos = Debug_event.lexing_position t.event.ev in
    Location.{ loc_start = pos; loc_end = pos; loc_ghost = false }
  else t.event.ev.ev_loc
