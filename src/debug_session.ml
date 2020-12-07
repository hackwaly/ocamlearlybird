let start rpc =
  let loop = Debug_rpc.start rpc in
  Lwt.async (fun () ->
    (try%lwt
      while%lwt true do
        Logs_lwt.debug (fun m -> m "state_uninitialized");%lwt
        let%lwt init_args, caps = Debug_state_uninitialized.run rpc in
        Logs_lwt.debug (fun m -> m "state_initialized");%lwt
        let%lwt (debug, terminate) = Debug_state_initialized.run ~init_args ~caps rpc in
        (match debug with
          | No_debug -> (
            Logs_lwt.debug (fun m -> m "state_no_debug");%lwt
            Debug_state_no_debug.run ~terminate rpc
          )
          | Debug {symbols; com} ->
            Logs_lwt.debug (fun m -> m "state_debug");%lwt
            Debug_state_debug.run ~terminate ~symbols ~com rpc
        );%lwt
        fst (Lwt.task ())
      done
    with Exit -> Lwt.return_unit);%lwt
    Logs_lwt.debug (fun m -> m "state_end");%lwt
    Lwt.cancel loop;
    Lwt.return_unit
  );
  (try%lwt loop with Lwt.Canceled -> Lwt.return_unit);%lwt
  Logs_lwt.debug (fun m -> m "loop end")
