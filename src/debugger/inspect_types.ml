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
  module_ : Debuginfo.module_;
  event : Debuginfo.event;
  mutable scopes : obj list;
  env : Env.t Lwt.t Lazy.t;
}

type scene = {
  report : Debugcom.report;
  frames : stack_frame array;
  obj_tbl : (int, obj) Hashtbl.t;
}

type ident = Ident of string | Qualified of string * ident

type expr = Lookup of ident | Field of expr * ident | Component of expr * int
