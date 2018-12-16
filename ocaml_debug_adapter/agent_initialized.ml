open Debug_adapter_protocol
open Debug_protocol
open Debug_protocol_ex
open Signatures

module Make (Args : sig
    val rpc : Rpc.t
    val replace_agent : (module AGENT) -> unit
    val init_args : Initialize_command.Request.Arguments.t
    val caps : Capabilities.t
  end) = struct

  include Args
  include Agent_null

  let getsockname sock =
    let addr = Lwt_unix.getsockname sock in
    match addr with
    | Unix.ADDR_UNIX addr -> addr
    | Unix.ADDR_INET (addr, port) ->
      Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port

  let with_chdir =
    let chdir_lock = Lwt_mutex.create () in
    fun cwd fn ->
      match cwd with
      | Some cwd ->
        Lwt_mutex.with_lock chdir_lock (fun () ->
          let%lwt cwd' = Lwt_unix.getcwd () in
          Lwt_unix.chdir cwd;%lwt
          (fn ())[%finally Lwt_unix.chdir cwd']
        )
      | None -> fn ()

  let spawn args sock =
    let open Launch_command.Request.Arguments in
    if args.console <> Console.Internal_console && init_args.supports_run_in_terminal_request then (
      let kind = match args.console with
        | Console.Integrated_terminal -> Run_in_terminal_command.Request.Arguments.Kind.Integrated
        | Console.External_terminal -> Run_in_terminal_command.Request.Arguments.Kind.External
        | Console.Internal_console -> assert false
      in
      let cwd = match args.cwd with
        | Some cwd -> cwd
        | None -> Filename.dirname args.program
      in
      let env = String_opt_dict.add "CAML_DEBUG_SOCKET" (
        match sock with
        | Some sock -> Some (getsockname sock)
        | None -> None
      ) args.env in
      (* TODO: *)
      let%lwt _ = Rpc.exec_command rpc (module Run_in_terminal_command) Run_in_terminal_command.Request.Arguments.{
        kind = Some kind; title = None; cwd; env; args = args.program :: args.arguments
      } in
      Lwt.return Agent_launched.In_terminal
    ) else (
      let env = (
        Unix.environment ()
        |> Array.to_list
        |> List.map (BatString.split ~by:"=")
        |> List.fold_left (fun dict (key, value) -> String_dict.add key value dict) String_dict.empty
      ) in
      let env = env |> match sock with
      | Some sock -> String_dict.add "CAML_DEBUG_SOCKET" (getsockname sock)
      | None -> String_dict.remove "CAML_DEBUG_SOCKET"
      in
      let env = String_opt_dict.fold (fun key value env ->
        match value with
        | Some value -> String_dict.add key value env
        | None -> String_dict.remove key env
      ) args.env env in
      let env = (
        env
        |> String_dict.bindings
        |> List.map (fun (key, value) -> key ^ "=" ^ value)
        |> Array.of_list
      ) in
      let%lwt proc = with_chdir args.cwd (fun () ->
        Lwt.return (Lwt_process.open_process_full ~env (
          "", args.program :: args.arguments |> Array.of_list)
        )
      ) in
      Lwt.return (Agent_launched.Process proc)
    )

  let launch_no_debug args =
    let%lwt proc = spawn args None in
    replace_agent (module Agent_launched_no_debug.Make (struct
        include Args
        let launch_args = args
        let proc = proc
      end));
    Lwt.return_ok ()

  let launch_debug args =
    let open Launch_command.Request.Arguments in
    let%lwt symbols = Symbols.load ~dot_merlins:args.dot_merlins args.program in
    match symbols with
    | None -> Lwt.fail_with "No debug symbols."
    | Some symbols -> (
        let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
        Lwt_unix.(bind sock Unix.(ADDR_INET (inet_addr_loopback, 0)));%lwt
        Lwt_unix.listen sock 1;
        let promise, resolver = Lwt.wait () in
        Lwt_util.async (fun () ->
          let%lwt fd, _ = Lwt_unix.accept sock in
          Lwt_unix.close sock;%lwt
          let in_chan = Lwt_io.(of_fd ~mode:Input fd) in
          let out_chan = Lwt_io.(of_fd ~mode:Output fd) in
          Lwt.wakeup resolver (Debug_conn.create in_chan out_chan);
          Lwt.return_unit
        );
        let%lwt proc = spawn args (Some sock) in
        let%lwt conn = promise in
        let%lwt pid = Debug_conn.initial conn in
        (* TODO: Move these code to suitable place *)
        Config.load_path := Symbols.all_dirs symbols;
        Envaux.reset_cache ();
        replace_agent (module Agent_launched_debug.Make (struct
            include Args
            let launch_args = args
            let proc = proc
            let symbols = symbols
            let conn = conn
            let pid = pid
          end));
        Lwt.return_ok ()
      )

  let launch_command args =
    let open Launch_command.Request.Arguments in
    if args.no_debug then (
      launch_no_debug args
    ) else (
      launch_debug args
    )
end
