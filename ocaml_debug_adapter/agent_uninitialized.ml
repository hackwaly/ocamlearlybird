open Debug_adapter_protocol
open Debug_protocol
open Signatures

module Make (Args : sig
    val rpc : Rpc.t
    val replace_agent : (module AGENT) -> unit
  end) = struct

  include Args
  include Agent_null

  let initialize_command args =
    let caps = Capabilities.make
        ~supports_configuration_done_request:true
        ~supports_loaded_sources_request:true
        ~supports_evaluate_for_hovers:true
        () in

    replace_agent (module Agent_initialized.Make (struct
        include Args
        let init_args = args
        let caps = caps
      end));

    Lwt.return_ok caps
end
