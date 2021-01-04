type module_ = {
  frag : int;
  id : string;
  source : string option;
  mutable events : event array;
}

and event = {
  module_ : module_;
  ev : Instruct.debug_event;
  env : Env.t Lwt.t Lazy.t;
}

val load : int -> string -> module_ list Lwt.t
(** [load frag file] Load debug info from file *)
