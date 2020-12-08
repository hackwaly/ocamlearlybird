open Astring
open Debug_protocol_ex

type debug =
  | Debug of {
    symbols : Debug_symbols.t;
    com : Debug_com.t;
  }
  | No_debug

let spawn ~rpc ?debug_sock ?env ?cwd prog args =
  let cwd = Option.value ~default:(Filename.dirname prog) cwd in
  let cmd = "", prog :: args |> Array.of_list in
  let curr_env = (
    Unix.environment ()
    |> Array.to_list
    |> List.filter_map (String.cut ~sep:"=")
    |> List.fold_left (fun dict (key, value) -> String_dict.add key value dict) String_dict.empty
  ) in
  if curr_env |> String_dict.mem "" then (
    Logs_lwt.warn (fun m -> m "CAML_DEBUG_SOCKET already in env")
  ) else Lwt.return_unit;%lwt
  let curr_env = match debug_sock with
    | None -> curr_env
    | Some sock -> (
      String_dict.add "CAML_DEBUG_SOCKET" (Lwt_util.getstringsockname sock) curr_env
    )
  in
  let env = List.fold_left (fun curr_env (k, v) ->
    match v with
    | None ->  String_dict.remove k curr_env
    | Some v -> String_dict.add k v curr_env
    ) curr_env (env |> Option.value ~default:String_opt_dict.empty |> String_opt_dict.bindings)
  in
  let%lwt proc = Lwt_util.with_chdir ~cwd (fun () ->
    let redir_output out_chan category () =
      Lwt_util.loop_read out_chan (fun content ->
        Debug_rpc.send_event rpc (module Output_event) Output_event.Payload.(
          make ~category:(Some category) ~output:content ()
        );
      )
    in
    let env = env
      |> String_dict.bindings
      |> List.map (fun (k, v) -> k ^ "=" ^ v)
      |> Array.of_list
    in
    let proc = Lwt_process.open_process_full ~env cmd in
    Lwt.async (redir_output proc#stdout Output_event.Payload.Category.Stdout);
    Lwt.async (redir_output proc#stderr Output_event.Payload.Category.Stderr);
    Lwt.return proc
  ) in
  Lwt.return (fun force ->
    if force then (
      proc#terminate;
      Lwt.return_unit
    ) else (
      if Sys.win32 then (
        Sys.command (Format.sprintf "taskkill /pid %d" proc#pid) |> ignore;
      ) else (
        proc#kill 2;
      );
      let%lwt _ = proc#status in
      Lwt.return_unit
    )
  )

let launch ~rpc arg =
  let open Launch_command.Arguments in
  let%lwt symbols =
    if arg.no_debug then
      Lwt.return_none
    else (
      let symbols_file = Option.value ~default:arg.program arg.symbols in
      Debug_symbols.load ~dot_merlins:arg.dot_merlins symbols_file
    )
  in
  if Option.is_none symbols && not arg.no_debug then (
    Logs_lwt.warn (fun m -> m "Symbols not found")
  ) else Lwt.return_unit;%lwt
  match symbols with
  | None -> (
    let%lwt terminate = spawn ~rpc ?cwd:arg.cwd arg.program arg.arguments in
    Lwt.return (No_debug, terminate)
  )
  | Some symbols -> (
    let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
    Lwt_unix.(bind sock Unix.(ADDR_INET (inet_addr_loopback, 0)));%lwt
    Lwt_unix.listen sock 1;
    let promise, resolver = Lwt.wait () in
    Lwt.async (fun () ->
      let%lwt fd, _ = Lwt_unix.accept sock in
      Lwt_unix.close sock;%lwt
      let in_chan = Lwt_io.(of_fd ~mode:Input fd) in
      let out_chan = Lwt_io.(of_fd ~mode:Output fd) in
      Lwt.wakeup resolver (Debug_com.create in_chan out_chan);
      Lwt.return_unit
    );
    let%lwt terminator = spawn ~rpc ~debug_sock:sock ?cwd:arg.cwd arg.program arg.arguments in
    let%lwt com = promise in
    Lwt.return (Debug {symbols; com}, terminator)
  )

let run ~init_args ~caps rpc =
  ignore init_args;
  ignore caps;
  let (promise, resolver) = Lwt.task () in
  let prevent_reenter () =
    Debug_rpc.remove_command_handler rpc (module Launch_command);
    Debug_rpc.remove_command_handler rpc (module Attach_command);
  in
  Debug_rpc.set_command_handler rpc (module Launch_command) (fun arg ->
    prevent_reenter ();
    let%lwt launched = launch ~rpc arg in
    Lwt.wakeup_later resolver launched;
    Lwt.return_unit
  );
  Debug_rpc.set_command_handler rpc (module Attach_command) (fun _ ->
    prevent_reenter ();
    Lwt.fail_with "Unsupported"
  );
  Debug_rpc.set_command_handler rpc (module Disconnect_command) (fun _ ->
    Debug_rpc.remove_command_handler rpc (module Disconnect_command);
    Lwt.wakeup_later_exn resolver Exit;
    Lwt.return_unit
  );
  promise
