open Debug_adapter_protocol
open Debug_protocol
open Debug_protocol_ex
open Signatures

type launched_process =
  | In_terminal
  | Process of Lwt_process.process_full

module Make (Args : sig
    val rpc : Rpc.t
    val replace_agent : (module AGENT) -> unit
    val init_args : Initialize_command.Request.Arguments.t
    val caps : Capabilities.t
    val launch_args : Launch_command.Request.Arguments.t
    val proc : launched_process
  end) = struct

  include Args
  include Agent_null

  let redir_output out_chan category () =
    Lwt_util.loop_read out_chan (fun content ->
      Rpc.emit_event rpc (module Output_event) Output_event.Body.(
        make ~category:(Some category) ~output:content ()
      );
    )

  let shutdown () =
    let%lwt () = match proc with
      | In_terminal -> Lwt.return_unit
      | Process proc -> (
          proc#terminate;
          match%lwt proc#status with
          | exception _ -> Lwt.return_unit
          | _ -> Lwt.return_unit
        )
    in
    Rpc.emit_event rpc (module Terminated_event) { restart = `Assoc [] }

  let disconnect_command _ =
    shutdown ();%lwt
    replace_agent (module Agent_disconnected);
    Lwt.return_ok ()

  let () =
    match proc with
    | In_terminal -> ()
    | Process proc ->
      Lwt_util.async (redir_output proc#stdout "stdout");
      Lwt_util.async (redir_output proc#stderr "stderr");
end
