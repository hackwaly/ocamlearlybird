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

let run ~launch_args ~dbg rpc =
  ignore launch_args;
  Lwt.pause ();%lwt
  Debug_rpc.set_command_handler rpc
    (module Pause_command)
    (fun _ ->
      Debugger.pause dbg);
  Debug_rpc.set_command_handler rpc
    (module Continue_command)
    (fun _ ->
      Debugger.run dbg;%lwt
      Lwt.return (Continue_command.Result.make ()));
  Debug_rpc.set_command_handler rpc
    (module Step_in_command)
    (fun _ ->
      Debugger.step_in dbg);
  Debug_rpc.set_command_handler rpc
    (module Step_out_command)
    (fun _ ->
      Debugger.step_out dbg);
  Debug_rpc.set_command_handler rpc
    (module Next_command)
    (fun _ ->
      Debugger.next dbg);
  Lwt.return ()

