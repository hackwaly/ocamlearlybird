open Debug_types

type t = {
  index : int;
  stack_pos : Sp.t;
  pc : int * int;
  event : Instruct.debug_event option;
  loc : source_range option;
  typenv : Typenv.t Lazy.t;
  globals : int Ident.Map.t Lazy.t;
}

exception No_debug_event
