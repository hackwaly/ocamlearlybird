open Ground

let on_connection ~client in_ out =
  Log.info (fun m -> m "Client %s connected" client);
  Adapter.serve in_ out;%lwt
  Log.info (fun m -> m "Client %s disconnected" client);
  Lwt_io.close out

let debug () =
  on_connection ~client:"stdio" Lwt_io.stdin Lwt_io.stdout

let serve port =
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let%lwt _ =
    Lwt_io.establish_server_with_client_address addr
      (fun addr (in_chan, out_chan) -> on_connection ~client:(Unix.Sockaddr.to_string addr) in_chan out_chan)
  in
  Log.info (fun m -> m "Debug adapter server listening at port %d" port);
  fst (Lwt.wait ())

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

(* Command line interface *)

open Cmdliner

let setup_log =
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let debug_command =
  let debug () = Lwt_main.run (debug ()) in
  Term.(const debug $ setup_log, info "debug")

let serve_command =
  let port_arg = Arg.(value & opt int 4711 & info [ "port" ]) in
  let serve port () = Lwt_main.run (serve port) in
  Term.(const serve $ port_arg $ setup_log, info "serve")

let default_command =
  Term.(ret (const (`Help (`Pager, None))), info "ocamlearybird")

let () =
  (* I don't know why:
   * Without this hook handler, there will be an fatal error after terminated event sent.
   * But after added this, It disappears.
   * `Fatal error: exception Lwt_io.Channel_closed("output")` *)
  (Lwt.async_exception_hook :=
     fun exn ->
       if exn <> Exit then (
         Printf.fprintf stderr "Async exception: %s\n" (Printexc.to_string exn);
         Printexc.print_backtrace stderr ));
  Term.(exit @@ eval_choice default_command [ debug_command; serve_command ])
