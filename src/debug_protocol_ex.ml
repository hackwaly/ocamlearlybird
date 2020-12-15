include Debug_protocol

module Launch_command = struct
  let type_ = Launch_command.type_

  module Arguments = struct
    module Console = struct
      type t = Internal_console | Integrated_terminal | External_terminal

      let of_yojson = function
        | `String "internalConsole" -> Result.Ok Internal_console
        | `String "integratedTerminal" -> Result.Ok Integrated_terminal
        | `String "externalTerminal" -> Result.Ok External_terminal
        | _ ->
            Result.Error "Debug_types.Launch_command.Request.Arguments.Console"

      let to_yojson = function
        | Internal_console -> `String "internalConsole"
        | Integrated_terminal -> `String "integratedTerminal"
        | External_terminal -> `String "externalTerminal"
    end

    type t = {
      no_debug : bool; [@default false] [@key "noDebug"]
      cwd : string option; [@default None]
      env : String_opt_dict.t; [@default String_opt_dict.empty]
      program : string;
      symbols : string option; [@default None]
      arguments : string list; [@default []]
      console : Console.t; [@default Console.Internal_console]
      stop_on_entry : bool; [@default false] [@key "stopOnEntry"]
      dot_merlins : string list; [@default []] [@key "dotMerlins"]
      __restart : Yojson.Safe.t option; [@default None]
    }
    [@@deriving yojson { strict = false }]
  end

  module Result = Launch_command.Result
end
