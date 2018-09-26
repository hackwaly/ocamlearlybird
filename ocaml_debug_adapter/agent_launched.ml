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

  let loop_read in_chan f =
    let action da =
      let open Lwt_io in
      let break = ref false in
      while%lwt not !break do
        if da.da_ptr < da.da_max then begin
          let content = Lwt_bytes.proxy da.da_buffer da.da_ptr (da.da_max - da.da_ptr) in
          da.da_ptr <- da.da_max;
          let content = Lwt_bytes.to_string content in
          f content
        end else begin
          let%lwt size = da.da_perform () in
          if size = 0 then break := true;
          Lwt.return_unit
        end
      done in
    Lwt_io.direct_access in_chan action

  let redir_output out_chan category () =
    loop_read out_chan (fun content ->
      Rpc.emit_event rpc (module Output_event) Output_event.Body.(
        make ~category:(Some category) ~output:content ()
      );
    )

  let shutdown () =
    match proc with
    | In_terminal -> Lwt.return_unit
    | Process proc -> (
        proc#terminate;
        match%lwt proc#status with
        | exception _ -> Lwt.return_unit
        | _ -> Lwt.return_unit
      )

  let disconnect_command _ =
    shutdown ();%lwt
    replace_agent (module Agent_disconnected);
    Lwt.return_ok ()

  let () =
    match proc with
    | In_terminal -> ()
    | Process proc ->
      Lwt.async (redir_output proc#stdout "stdout");
      Lwt.async (redir_output proc#stderr "stderr");
end
