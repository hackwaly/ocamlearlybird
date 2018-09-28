[@@@warning "-39"]

open Debug_adapter_protocol
open Debug_protocol

module Launch_command = struct
  let name = Launch_command.name
  module Request = struct
    module Arguments = struct
      module Console = struct
        type t =
            Internal_console
          | Integrated_terminal
          | External_terminal

        let of_yojson = function
            `String "internalConsole" -> Result.Ok Internal_console
          | `String "integratedTerminal" -> Result.Ok Integrated_terminal
          | `String "externalTerminal" -> Result.Ok External_terminal
          | _ -> Result.Error "Debug_types.Launch_command.Request.Arguments.Console"

        let to_yojson = function
            Internal_console -> `String "internalConsole"
          | Integrated_terminal -> `String "integratedTerminal"
          | External_terminal -> `String "externalTerminal"
      end

      type t = {
        no_debug : bool [@default false] [@key "noDebug"];
        cwd : string option [@default None];
        env : String_opt_dict.t [@default String_opt_dict.empty];
        program : string;
        arguments : string list [@default []];
        console : Console.t [@default Console.Internal_console];
        stop_on_entry : bool [@default false] [@key "stopOnEntry"];
        __restart: Yojson.Safe.json option [@default None];
      } [@@deriving yojson { strict = false }]
    end
  end
  module Response = Launch_command.Response
end
