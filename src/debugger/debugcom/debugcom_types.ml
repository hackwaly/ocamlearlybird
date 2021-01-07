type protocol_version = OCaml_400 | OCaml_410 [@@deriving show]

exception Checkpoint_failure

type debug_info = { eventlists : Instruct.debug_event list array [@opaque] }
[@@deriving show]

type execution_summary =
  | Event
  | Breakpoint
  | Exited
  | Trap
  | Uncaught_exc
  | Code_debug_info of debug_info
  | Code_loaded of int
  | Code_unloaded of int
[@@deriving show]

type report = {
  rep_type : execution_summary;
  rep_event_count : int64;
  rep_stack_pointer : int;
  rep_program_pointer : Pc.t;
}
[@@deriving show]

type fork_mode = Fork_child | Fork_parent [@@deriving show]
