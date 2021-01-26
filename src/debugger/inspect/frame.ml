open Debug_types

type t = {
  index : int;
  stack_pos : int;
  pc : int * int;
  event : Instruct.debug_event option;
  loc : source_range option;
  typenv : Typenv.t Lazy.t;
}

exception No_debug_event

