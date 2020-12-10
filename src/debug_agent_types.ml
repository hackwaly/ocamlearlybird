type pc = {
  frag : int; (** A number to identify bytecodes, such as program or dll. Program's is 0. Dll's is reported by [Code_loaded] and [Code_unloaded] *)
  pos : int;
}

type follow_fork_mode =
  | Fork_parent
  | Fork_child

type execution_summary =
  | Event
  | Breakpoint
  | Exited
  | Trap_barrier
  | Uncaught_exc
  | Debug_info of Instruct.debug_event list array
  | Code_loaded of int (** [Code_loaded frag] Dll is loaded *)
  | Code_unloaded of int (** [Code_unloaded frag] Dll is unloaded *)

type report = {
  rep_type : execution_summary;
  rep_event_count : int64;
  rep_stack_pointer : int;
  rep_program_pointer : pc;
}
