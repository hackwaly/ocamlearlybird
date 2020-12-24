type frame_scope_kind = [ `Local | `Heap | `Rec ]

type scope_kind = [ frame_scope_kind | `Global ]

type value =
  | Int of int
  | Double of float
  | Bool of bool
  | Char of char
  | String of string
  | Scope of { scene_id : int64; index : int; kind : scope_kind }
  | Unknown

type obj = {
  id : int;
  name : string;
  value : value;
  members : obj list Lwt.t Lazy.t;
}

type ident = Ident of string | Qualified of string * ident

type expr = Lookup of ident | Field of expr * ident | Component of expr * int
