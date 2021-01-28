(**
 * Copyright (C) 2021 Yuxiang Wen
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

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
      __restart : Yojson.Safe.t option; [@default None]
      name : string option; [@default None]
      cwd : string option; [@default None]
      env : String_opt_dict.t; [@default String_opt_dict.empty]
      stop_on_entry : bool; [@default false] [@key "stopOnEntry"]
      program : string;
      arguments : string list; [@default []]
      console : Console.t; [@default Console.Integrated_terminal]
      source_dirs : string list; [@default []]
      follow_fork_mode : Follow_fork_mode.t; [@default Follow_fork_mode.Fork_parent] [@key "followForkMode"]
      only_debug_glob : string option; [@default None] [@key "onlyDebugGlob"]
      yield_steps : int option; [@default None] [@key "yieldSteps"]
      _debug_log : string option; [@default None] [@key "_debugLog"]
    }
    [@@deriving yojson { strict = false }]
  end

  module Result = Launch_command.Result
end
