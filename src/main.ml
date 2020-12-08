let on_connection in_ out =
  let rpc = Debug_rpc.create ~in_ ~out () in
  Debug_session.start rpc;%lwt
  Logs_lwt.debug (fun m -> m "close");%lwt
  Lwt_io.close out

let serve port =
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let%lwt _ = Lwt_io.establish_server_with_client_address addr (
    fun _ (in_chan, out_chan) -> on_connection in_chan out_chan
  ) in
  Logs.app (fun m -> m "Debug adapter server listening at port %d" port);
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

let serve_command =
  let port_arg = Arg.(value & opt int 4711 & info ["port"]) in
  let serve port () =
    Lwt_main.run (serve port)
  in
  Term.(const serve $ port_arg $ setup_log, info "serve")

let default_cmd =
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "ocamlearybird"

let () =
  Term.(exit @@ eval_choice default_cmd [serve_command])