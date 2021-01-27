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

    module Follow_fork_mode = struct
      type t = Fork_parent | Fork_child

      let of_yojson = function
        | `String "forkParent" -> Result.Ok Fork_parent
        | `String "forkChild" -> Result.Ok Fork_child
        | _ ->
            Result.Error "Debug_types.Launch_command.Request.Arguments.Console"

      let to_yojson = function
        | Fork_parent -> `String "forkParent"
        | Fork_child -> `String "forkChild"
    end

    type t = {
      no_debug : bool; [@default false] [@key "noDebug"]
      __restart : Yojson.Safe.t option; [@default None]
      name : string option; [@default None]
      cwd : string option; [@default None]
      env : String_opt_dict.t; [@default String_opt_dict.empty]
      stop_on_entry : bool; [@default false] [@key "stopOnEntry"]
      program : string;
      arguments : string list; [@default []]
      symbols : string option; [@default None]
      console : Console.t; [@default Console.Integrated_terminal]
      source_dirs : string list; [@default []]
      follow_fork_mode : Follow_fork_mode.t; [@default Follow_fork_mode.Fork_parent] [@key "followForkMode"]
      only_debug_glob : string option; [@default None] [@key "onlyDebugGlob"]
      yield_steps : int option; [@default None] [@key "yieldSteps"]
    }
    [@@deriving yojson { strict = false }]
  end

  module Result = Launch_command.Result
end
