open Debug_adapter_protocol
open Debug_protocol
open Debug_protocol_ex

type launched_process =
  | In_terminal
  | Process of Lwt_process.process_full

module type SESSION = sig
  val id : int
  val start : unit -> unit Lwt.t
  val shutdown : unit -> unit Lwt.t
end

module type AGENT = sig
  val shutdown : unit -> unit Lwt.t

  val attach_command : Attach_command.Request.Arguments.t -> (Attach_command.Response.Body.t, string * Message.t option) result Lwt.t
  val completions_command : Completions_command.Request.Arguments.t -> (Completions_command.Response.Body.t, string * Message.t option) result Lwt.t
  val configuration_done_command : Configuration_done_command.Request.Arguments.t -> (Configuration_done_command.Response.Body.t, string * Message.t option) result Lwt.t
  val continue_command : Continue_command.Request.Arguments.t -> (Continue_command.Response.Body.t, string * Message.t option) result Lwt.t
  val disconnect_command : Disconnect_command.Request.Arguments.t -> (Disconnect_command.Response.Body.t, string * Message.t option) result Lwt.t
  val evaluate_command : Evaluate_command.Request.Arguments.t -> (Evaluate_command.Response.Body.t, string * Message.t option) result Lwt.t
  val exception_info_command : Exception_info_command.Request.Arguments.t -> (Exception_info_command.Response.Body.t, string * Message.t option) result Lwt.t
  val goto_command : Goto_command.Request.Arguments.t -> (Goto_command.Response.Body.t, string * Message.t option) result Lwt.t
  val goto_targets_command : Goto_targets_command.Request.Arguments.t -> (Goto_targets_command.Response.Body.t, string * Message.t option) result Lwt.t
  val initialize_command : Initialize_command.Request.Arguments.t -> (Initialize_command.Response.Body.t, string * Message.t option) result Lwt.t
  val launch_command : Launch_command.Request.Arguments.t -> (Launch_command.Response.Body.t, string * Message.t option) result Lwt.t
  val loaded_sources_command : Loaded_sources_command.Request.Arguments.t -> (Loaded_sources_command.Response.Body.t, string * Message.t option) result Lwt.t
  val modules_command : Modules_command.Request.Arguments.t -> (Modules_command.Response.Body.t, string * Message.t option) result Lwt.t
  val next_command : Next_command.Request.Arguments.t -> (Next_command.Response.Body.t, string * Message.t option) result Lwt.t
  val pause_command : Pause_command.Request.Arguments.t -> (Pause_command.Response.Body.t, string * Message.t option) result Lwt.t
  val restart_command : Restart_command.Request.Arguments.t -> (Restart_command.Response.Body.t, string * Message.t option) result Lwt.t
  val restart_frame_command : Restart_frame_command.Request.Arguments.t -> (Restart_frame_command.Response.Body.t, string * Message.t option) result Lwt.t
  val reverse_continue_command : Reverse_continue_command.Request.Arguments.t -> (Reverse_continue_command.Response.Body.t, string * Message.t option) result Lwt.t
  val scopes_command : Scopes_command.Request.Arguments.t -> (Scopes_command.Response.Body.t, string * Message.t option) result Lwt.t
  val set_breakpoints_command : Set_breakpoints_command.Request.Arguments.t -> (Set_breakpoints_command.Response.Body.t, string * Message.t option) result Lwt.t
  val set_exception_breakpoints_command : Set_exception_breakpoints_command.Request.Arguments.t -> (Set_exception_breakpoints_command.Response.Body.t, string * Message.t option) result Lwt.t
  val set_expression_command : Set_expression_command.Request.Arguments.t -> (Set_expression_command.Response.Body.t, string * Message.t option) result Lwt.t
  val set_function_breakpoints_command : Set_function_breakpoints_command.Request.Arguments.t -> (Set_function_breakpoints_command.Response.Body.t, string * Message.t option) result Lwt.t
  val set_variable_command : Set_variable_command.Request.Arguments.t -> (Set_variable_command.Response.Body.t, string * Message.t option) result Lwt.t
  val source_command : Source_command.Request.Arguments.t -> (Source_command.Response.Body.t, string * Message.t option) result Lwt.t
  val stack_trace_command : Stack_trace_command.Request.Arguments.t -> (Stack_trace_command.Response.Body.t, string * Message.t option) result Lwt.t
  val step_back_command : Step_back_command.Request.Arguments.t -> (Step_back_command.Response.Body.t, string * Message.t option) result Lwt.t
  val step_in_command : Step_in_command.Request.Arguments.t -> (Step_in_command.Response.Body.t, string * Message.t option) result Lwt.t
  val step_in_targets_command : Step_in_targets_command.Request.Arguments.t -> (Step_in_targets_command.Response.Body.t, string * Message.t option) result Lwt.t
  val step_out_command : Step_out_command.Request.Arguments.t -> (Step_out_command.Response.Body.t, string * Message.t option) result Lwt.t
  val terminate_command : Terminate_command.Request.Arguments.t -> (Terminate_command.Response.Body.t, string * Message.t option) result Lwt.t
  val terminate_threads_command : Terminate_threads_command.Request.Arguments.t -> (Terminate_threads_command.Response.Body.t, string * Message.t option) result Lwt.t
  val threads_command : Threads_command.Request.Arguments.t -> (Threads_command.Response.Body.t, string * Message.t option) result Lwt.t
  val variables_command : Variables_command.Request.Arguments.t -> (Variables_command.Response.Body.t, string * Message.t option) result Lwt.t
end
