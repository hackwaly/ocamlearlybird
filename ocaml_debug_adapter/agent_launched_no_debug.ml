open Debug_adapter_protocol
open Debug_protocol
open Debug_protocol_ex
open Signatures

module Make (Args : sig
    val rpc : Rpc.t
    val replace_agent : (module AGENT) -> unit
    val init_args : Initialize_command.Request.Arguments.t
    val caps : Capabilities.t
    val launch_args : Launch_command.Request.Arguments.t
    val proc : Agent_launched.launched_process
  end) = struct

  include Args
  include Agent_null
  include Agent_launched.Make (Args)

  let loaded_sources_command _ =
    Lwt.return_ok Loaded_sources_command.Response.Body.{
      sources = [];
    }

  let () =
    match proc with
    | In_terminal -> ()
    | Process proc ->
      Lwt_util.async (fun () ->
        let%lwt () = match%lwt proc#status with
          | exception _ -> Lwt.return_unit
          | _ -> Lwt.return_unit
        in
        Rpc.emit_event rpc (module Terminated_event) { restart = `Assoc [] }
      )
end
