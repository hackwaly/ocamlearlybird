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

open Debug_protocol_ex

let run rpc =
  let promise, resolver = Lwt.task () in
  let prevent_reenter () =
    Debug_rpc.remove_command_handler rpc (module Initialize_command)
  in
  Debug_rpc.set_command_handler rpc
    (module Initialize_command)
    (fun arg ->
      prevent_reenter ();
      let caps =
        Capabilities.(
          make ~supports_terminate_request:(Some true)
            ~supports_configuration_done_request:(Some true)
            ~supports_loaded_sources_request:(Some true)
            ~supports_delayed_stack_trace_loading:(Some true)
            ~supports_breakpoint_locations_request:(Some true)
            ~supports_value_formatting_options:(Some true) ())
      in
      Lwt.wakeup_later resolver (arg, caps);
      Lwt.return caps);
  promise
