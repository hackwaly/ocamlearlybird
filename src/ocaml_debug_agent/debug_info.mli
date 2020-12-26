type event = { frag : int; ev : Instruct.debug_event; env : Env.t Lwt.t Lazy.t }

type module_ = {
  frag : int;
  id : string;
  source : string Lwt.t Lazy.t;
  events : event array;
}

val load : int -> string -> module_ list Lwt.t
(** [load frag file] Load debug info from file *)
