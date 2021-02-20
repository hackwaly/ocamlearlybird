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
open Path_glob

let spawn_console ~rpc ?name ?env ?cwd prog args =
  ignore name;
  let cwd = Option.value ~default:(Filename.dirname prog) cwd in
  let cmd = ("", prog :: args |> Array.of_list) in
  let curr_env =
    Unix.environment () |> Array.to_seq
    |> Seq.map (String.cut_on_char '=')
    |> Seq.fold_left
         (fun dict (key, value) -> String_dict.add key value dict)
         String_dict.empty
  in
  let env =
    List.fold_left
      (fun curr_env (k, v) ->
        match v with
        | None -> String_dict.remove k curr_env
        | Some v -> String_dict.add k v curr_env)
      curr_env
      (env
      |> Option.value ~default:String_opt_dict.empty
      |> String_opt_dict.bindings)
  in
  Lwt_unix.with_chdir ~cwd (fun () ->
      let redir_output out_chan category () =
        Lwt_io.loop_read out_chan (fun content ->
            Debug_rpc.send_event rpc
              (module Output_event)
              Output_event.Payload.(
                make ~category:(Some category) ~output:content ()))
      in
      let env =
        env |> String_dict.bindings
        |> List.map (fun (k, v) -> k ^ "=" ^ v)
        |> Array.of_list
      in
      let proc = Lwt_process.open_process_full ~env cmd in
      Lwt.async (redir_output proc#stdout Output_event.Payload.Category.Stdout);
      Lwt.async (redir_output proc#stderr Output_event.Payload.Category.Stderr);
      Lwt.return ());%lwt
  Lwt.return ()

let spawn_terminal ~kind ~rpc ?name ?env ?cwd prog args =
  let cwd = match cwd with Some cwd -> cwd | None -> Filename.dirname prog in
  Lwt.async (fun () ->
      let%lwt _ =
        Debug_rpc.exec_command rpc
          (module Run_in_terminal_command)
          Run_in_terminal_command.Arguments.
            { kind = Some kind; title = name; cwd; env; args = prog :: args }
      in
      Lwt.return ());
  Lwt.return ()

let spawn ~kind ~rpc ?name ?env ?cwd prog args =
  let open Launch_command.Arguments in
  match kind with
  | Console.Internal_console -> spawn_console ~rpc ?env ?cwd prog args
  | Integrated_terminal ->
      spawn_terminal ~kind:Run_in_terminal_command.Arguments.Kind.Integrated
        ~rpc ?name ?env ?cwd prog args
  | External_terminal ->
      spawn_terminal ~kind:Run_in_terminal_command.Arguments.Kind.External ~rpc
        ?name ?env ?cwd prog args

let launch ~rpc ~init_args ~capabilities ~launch_args =
  ignore init_args;
  ignore capabilities;
  let open Launch_command.Arguments in
  let open Initialize_command.Arguments in
  if launch_args._debug_log |> Option.is_some then (
    Logs.set_level (Some Debug);
    let file = open_out (launch_args._debug_log |> Option.get) in
    let fmt = Format.formatter_of_out_channel file in
    Logs.set_reporter (Logs_fmt.reporter ~app:fmt ~dst:fmt ()));
  let kind =
    if init_args.supports_run_in_terminal_request |> Option.value ~default:false
    then launch_args.console
    else Console.Internal_console
  in
  let debug_sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  Lwt_unix.(bind debug_sock Unix.(ADDR_INET (inet_addr_loopback, 0)));%lwt
  Lwt_unix.listen debug_sock 1;
  let env =
    launch_args.env
    |> String_opt_dict.add "CAML_DEBUG_SOCKET"
         (Some (debug_sock |> Lwt_unix.getsockname |> Unix.Sockaddr.to_string))
  in
  spawn ~kind ~rpc ?name:launch_args.name ~env ?cwd:launch_args.cwd
    launch_args.program launch_args.arguments;%lwt
  let%lwt dbg =
    let debug_filter =
      let globber =
        let open Option in
        let* glob = launch_args.only_debug_glob in
        try Glob.parse glob |> return with _ -> None
      in
      match globber with
      | None -> fun _ -> true
      | Some globber -> fun path -> Glob.eval globber path
    in
    Debugger.init
      (Debugger.make_options ~debug_sock ~symbols_file:launch_args.program
         ?yield_steps:launch_args.yield_steps
         ~follow_fork_mode:
           (match launch_args.follow_fork_mode with
           | Fork_parent -> `Fork_parent
           | Fork_child -> `Fork_child)
         ~debug_filter ())
  in
  Lwt.return (launch_args, dbg)

let run ~init_args ~capabilities rpc =
  let promise, resolver = Lwt.task () in
  let prevent_reenter () =
    Debug_rpc.remove_command_handler rpc (module Launch_command);
    Debug_rpc.remove_command_handler rpc (module Attach_command)
  in
  Debug_rpc.set_command_handler rpc
    (module Launch_command)
    (fun launch_args ->
      prevent_reenter ();
      let%lwt launched = launch ~rpc ~init_args ~capabilities ~launch_args in
      Lwt.wakeup_later resolver launched;
      Lwt.return_unit);
  Debug_rpc.set_command_handler rpc
    (module Attach_command)
    (fun _ ->
      prevent_reenter ();
      Lwt.fail_with "Unsupported");
  Debug_rpc.set_command_handler rpc
    (module Disconnect_command)
    (fun _ ->
      Debug_rpc.remove_command_handler rpc (module Disconnect_command);
      Lwt.wakeup_later_exn resolver Exit;
      Lwt.return_unit);
  promise
