open Debug_adapter_protocol
open Debug_protocol
open Debug_protocol_ex
open Signatures

type t = (module SESSION)

let new_session_id =
  let next_session_id = ref 0 in
  fun () ->
    let session_id = !next_session_id in
    incr next_session_id;
    session_id

let id (module Session : SESSION) = Session.id

let create in_chan out_chan =
  let module Session = struct
    let id = new_session_id ()
    let rpc = Rpc.create in_chan out_chan
    let agent = ref (module Agent_null : AGENT)
    let replace_agent agent' = agent := agent'

    let start () =
      Rpc.start rpc

    let shutdown () =
      let (module Agent) = !agent in Agent.shutdown ()

    let () =
      agent := (module Agent_uninitialized.Make (struct
          let rpc = rpc
          let replace_agent = replace_agent
        end));
      Rpc.handle_command rpc (module Attach_command) (fun args -> let (module Agent) = !agent in Agent.attach_command args);
      Rpc.handle_command rpc (module Completions_command) (fun args -> let (module Agent) = !agent in Agent.completions_command args);
      Rpc.handle_command rpc (module Configuration_done_command) (fun args -> let (module Agent) = !agent in Agent.configuration_done_command args);
      Rpc.handle_command rpc (module Continue_command) (fun args -> let (module Agent) = !agent in Agent.continue_command args);
      Rpc.handle_command rpc (module Disconnect_command) (fun args -> let (module Agent) = !agent in Agent.disconnect_command args);
      Rpc.handle_command rpc (module Evaluate_command) (fun args -> let (module Agent) = !agent in Agent.evaluate_command args);
      Rpc.handle_command rpc (module Exception_info_command) (fun args -> let (module Agent) = !agent in Agent.exception_info_command args);
      Rpc.handle_command rpc (module Goto_command) (fun args -> let (module Agent) = !agent in Agent.goto_command args);
      Rpc.handle_command rpc (module Goto_targets_command) (fun args -> let (module Agent) = !agent in Agent.goto_targets_command args);
      Rpc.handle_command rpc (module Initialize_command) (fun args -> let (module Agent) = !agent in Agent.initialize_command args);
      Rpc.handle_command rpc (module Launch_command) (fun args -> let (module Agent) = !agent in Agent.launch_command args);
      Rpc.handle_command rpc (module Loaded_sources_command) (fun args -> let (module Agent) = !agent in Agent.loaded_sources_command args);
      Rpc.handle_command rpc (module Modules_command) (fun args -> let (module Agent) = !agent in Agent.modules_command args);
      Rpc.handle_command rpc (module Next_command) (fun args -> let (module Agent) = !agent in Agent.next_command args);
      Rpc.handle_command rpc (module Pause_command) (fun args -> let (module Agent) = !agent in Agent.pause_command args);
      Rpc.handle_command rpc (module Restart_command) (fun args -> let (module Agent) = !agent in Agent.restart_command args);
      Rpc.handle_command rpc (module Restart_frame_command) (fun args -> let (module Agent) = !agent in Agent.restart_frame_command args);
      Rpc.handle_command rpc (module Reverse_continue_command) (fun args -> let (module Agent) = !agent in Agent.reverse_continue_command args);
      Rpc.handle_command rpc (module Scopes_command) (fun args -> let (module Agent) = !agent in Agent.scopes_command args);
      Rpc.handle_command rpc (module Set_breakpoints_command) (fun args -> let (module Agent) = !agent in Agent.set_breakpoints_command args);
      Rpc.handle_command rpc (module Set_exception_breakpoints_command) (fun args -> let (module Agent) = !agent in Agent.set_exception_breakpoints_command args);
      Rpc.handle_command rpc (module Set_expression_command) (fun args -> let (module Agent) = !agent in Agent.set_expression_command args);
      Rpc.handle_command rpc (module Set_function_breakpoints_command) (fun args -> let (module Agent) = !agent in Agent.set_function_breakpoints_command args);
      Rpc.handle_command rpc (module Set_variable_command) (fun args -> let (module Agent) = !agent in Agent.set_variable_command args);
      Rpc.handle_command rpc (module Source_command) (fun args -> let (module Agent) = !agent in Agent.source_command args);
      Rpc.handle_command rpc (module Stack_trace_command) (fun args -> let (module Agent) = !agent in Agent.stack_trace_command args);
      Rpc.handle_command rpc (module Step_back_command) (fun args -> let (module Agent) = !agent in Agent.step_back_command args);
      Rpc.handle_command rpc (module Step_in_command) (fun args -> let (module Agent) = !agent in Agent.step_in_command args);
      Rpc.handle_command rpc (module Step_in_targets_command) (fun args -> let (module Agent) = !agent in Agent.step_in_targets_command args);
      Rpc.handle_command rpc (module Step_out_command) (fun args -> let (module Agent) = !agent in Agent.step_out_command args);
      Rpc.handle_command rpc (module Terminate_command) (fun args -> let (module Agent) = !agent in Agent.terminate_command args);
      Rpc.handle_command rpc (module Terminate_threads_command) (fun args -> let (module Agent) = !agent in Agent.terminate_threads_command args);
      Rpc.handle_command rpc (module Threads_command) (fun args -> let (module Agent) = !agent in Agent.threads_command args);
      Rpc.handle_command rpc (module Variables_command) (fun args -> let (module Agent) = !agent in Agent.variables_command args)
  end in
  (module Session : SESSION)

let start (module Session : SESSION) =
  Session.start ()

let shutdown (module Session : SESSION) =
  Session.shutdown ()
