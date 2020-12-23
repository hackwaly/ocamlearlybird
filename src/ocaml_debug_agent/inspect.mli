type scope = Local | Heap | Rec | Global

type value =
  | Int of int
  | Double of float
  | Bool of bool
  | Char of char
  | String of string
  | Module of Symbols.Module.t
  | Scope of scope
  | Unknown

type t

type obj = { scene_id : int64; value : value; name : string }

type ident = Ident of string | Qualified of string * ident

type expr = Lookup of ident | Field of expr * ident | Component of expr * int

val create :
  symbols:Symbols.t ->
  remote_debugger:(module Remote_debugger.S) ->
  conn:Remote_debugger.conn ->
  unit ->
  t

val is_valid : t -> obj -> bool

val update_scene : t -> Remote_debugger.report -> unit

val scope_obj : t -> scope -> obj Lwt.t

val list_obj : t -> obj -> obj list Lwt.t
