type frame_scope_kind = [ `Stack | `Heap ]

type scope_kind = [ frame_scope_kind | `Global ]

type value =
  | Int of int
  | Double of float
  | Bool of bool
  | Char of char
  | String of string
  | Scope of { scene_id : int64; index : int; kind : scope_kind }
  | Function of { location : Location.t }
  | Unknown

type obj = {
  id : int;
  name : string;
  value : value;
  structured : bool;
  mutable members : obj list Lwt.t Lazy.t;
}

type stack_frame = {
  index : int;
  stack_pos : int;
  module_ : Symbols.Module.t;
  event : Instruct.debug_event;
  mutable scopes : obj list;
  env : Env.t Lazy.t;
}

module Stack_frame = struct
  type t = stack_frame = {
    index : int;
    stack_pos : int;
    module_ : Symbols.Module.t;
    event : Instruct.debug_event;
    mutable scopes : obj list;
    env : Env.t Lazy.t;
  }

  let stacksize t = t.event.ev_stacksize

  let defname t = t.event.ev_defname

  let module_ t = t.module_

  let pc t = { Pc.frag = (Module.frag t.module_); pos = t.event.ev_pos }

  let loc t =
    if t.index = 0 then
      let pos = Debug_event.lexing_position t.event in
      Location.{ loc_start = pos; loc_end = pos; loc_ghost = false }
    else t.event.ev_loc
end

type scene = {
  report : Debugcom.report;
  frames : Stack_frame.t array;
  obj_tbl : (int, obj) Hashtbl.t;
}

type ident = Ident of string | Qualified of string * ident

type expr = Lookup of ident | Field of expr * ident | Component of expr * int
