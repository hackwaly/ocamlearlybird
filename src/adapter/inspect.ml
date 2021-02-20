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

open Ground
open Debug_protocol_ex
open Debugger

let run ~launch_args ~dbg rpc =
  ignore launch_args;
  let alloc_handle = Unique_id.make_alloc 1 in
  let frame_tbl = Hashtbl.create 0 in
  let value_tbl = Hashtbl.create 0 in
  let process_state_changes () =
    let process status =
      Hashtbl.reset frame_tbl;
      Hashtbl.reset value_tbl;
      match status with
      | Running -> Lwt.return ()
      | Stopped (Exited | Uncaught_exc) ->
          Debug_rpc.send_event rpc
            (module Terminated_event)
            Terminated_event.Payload.(make ())
      | Stopped Entry ->
          Debug_rpc.send_event rpc
            (module Stopped_event)
            Stopped_event.Payload.(make ~reason:Entry ~thread_id:(Some 0) ())
      | Stopped Breakpoint ->
          Debug_rpc.send_event rpc
            (module Stopped_event)
            Stopped_event.Payload.(
              make ~reason:Breakpoint ~thread_id:(Some 0) ())
      | Stopped Step ->
          Debug_rpc.send_event rpc
            (module Stopped_event)
            Stopped_event.Payload.(make ~reason:Step ~thread_id:(Some 0) ())
      | Stopped Pause ->
          Debug_rpc.send_event rpc
            (module Stopped_event)
            Stopped_event.Payload.(make ~reason:Pause ~thread_id:(Some 0) ())
    in
    Debugger.state dbg |> Lwt_react.S.changes |> Lwt_react.E.to_stream
    |> Lwt_stream.iter_s process
  in
  Debug_rpc.set_command_handler rpc
    (module Loaded_sources_command)
    (fun () ->
      let sources = Debugger.get_sources dbg in
      let sources =
        sources |> List.map (fun source -> Source.make ~path:(Some source) ())
      in
      Loaded_sources_command.Result.make ~sources () |> Lwt.return);
  Debug_rpc.set_command_handler rpc
    (module Threads_command)
    (fun () ->
      let main_thread = Thread.make ~id:0 ~name:"main" in
      Lwt.return (Threads_command.Result.make ~threads:[ main_thread ] ()));
  Debug_rpc.set_command_handler rpc
    (module Stack_trace_command)
    (fun arg ->
      let%lwt frames =
        Debugger.get_frames ?start:arg.start_frame ?count:arg.levels dbg
      in
      let stack_frames =
        frames
        |> List.map (fun frame ->
               let source, (line, column), (end_line, end_column) =
                 match frame.loc with
                 | None -> (None, (0, 0), (0, 0))
                 | Some loc ->
                     ( Some (Source.make ~path:(Some loc.source) ()),
                       loc.pos,
                       loc.end_ )
               in
               Hashtbl.replace frame_tbl frame.index frame;
               Stack_frame.make ~id:frame.index
                 ~name:(frame.name |> Option.value ~default:"??")
                 ~source ~line ~column ~end_line:(Some end_line)
                 ~end_column:(Some end_column) ())
      in
      Lwt.return Stack_trace_command.Result.(make ~stack_frames ()));
  Debug_rpc.set_command_handler rpc
    (module Scopes_command)
    (fun arg ->
      let scopes =
        match Hashtbl.find_opt frame_tbl arg.frame_id with
        | None -> []
        | Some frame -> frame.scopes
      in
      let scopes =
        scopes
        |> List.map (fun (name, scope) ->
               let handle = alloc_handle () in
               Hashtbl.replace value_tbl handle scope;
               Scope.make ~name ~variables_reference:handle ~expensive:false ())
      in
      Lwt.return (Scopes_command.Result.make ~scopes ()));
  Debug_rpc.set_command_handler rpc
    (module Variables_command)
    (fun arg ->
      let%lwt variables =
        match Hashtbl.find_opt value_tbl arg.variables_reference with
        | None -> Lwt.return []
        | Some value -> (
            match arg.filter with
            | None ->
                assert (value#num_indexed = 0);
                value#list_named
            | Some Named -> value#list_named
            | Some Indexed ->
                let start = arg.start |> Option.value ~default:0 in
                let end_ =
                  (match arg.count with
                  | Some count -> start + count
                  | None -> value#num_indexed)
                  - 1
                in
                Seq.int_range ~start ~end_ ()
                |> List.of_seq
                |> Lwt_list.map_s (fun i ->
                       let%lwt obj = value#get_indexed i in
                       Lwt.return (string_of_int i, obj)))
      in
      let variables =
        variables
        |> List.map (fun (name, value) ->
               let num_named = value#num_named in
               let num_indexed = value#num_indexed in
               let is_complex =
                 num_indexed > 0 || num_named > 0 || num_named = -1
               in
               let handle = if is_complex then alloc_handle () else 0 in
               Hashtbl.replace value_tbl handle value;
               Variable.make ~name ~value:value#to_short_string
                 ~variables_reference:handle
                 ~named_variables:
                   (if num_named = -1 then None else Some num_named)
                 ~indexed_variables:(Some num_indexed) ())
      in
      Lwt.return (Variables_command.Result.make ~variables ()));
  Lwt.join [ process_state_changes () ]
