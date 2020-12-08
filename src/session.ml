let start rpc =
  let loop = Debug_rpc.start rpc in
  Lwt.async (fun () ->
    (try%lwt
      Logs_lwt.debug (fun m -> m "state_uninitialized");%lwt
      let%lwt init_args, caps = State_uninitialized.run rpc in
      Logs_lwt.debug (fun m -> m "state_initialized");%lwt
      let%lwt (debug, terminate) = State_initialized.run ~init_args ~caps rpc in
      (match debug with
        | No_debug -> (
          Logs_lwt.debug (fun m -> m "state_no_debug");%lwt
          State_no_debug.run ~terminate rpc
        )
        | Debug {symbols; com} ->
          Logs_lwt.debug (fun m -> m "state_debug");%lwt
          State_debug.run ~terminate ~symbols ~com rpc
      );%lwt
      fst (Lwt.task ())
    with Exit -> Lwt.return_unit);%lwt
    Logs_lwt.debug (fun m -> m "state_end");%lwt
    Lwt.cancel loop;
    Lwt.return_unit
  );
  (try%lwt loop with Lwt.Canceled -> Lwt.return_unit);%lwt
  Logs_lwt.debug (fun m -> m "loop end")
