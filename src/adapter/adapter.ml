open Ground

let serve in_ out =
  let rpc = Debug_rpc.create ~in_ ~out () in
  let cancel = ref (fun () -> ()) in
  Lwt.async (fun () ->
      ( try%lwt
          Log.debug (fun m -> m "state_uninitialized");
          let%lwt init_args, capabilities = State_uninitialized.run rpc in
          Log.debug (fun m -> m "state_initialized");
          let%lwt launch_args, dbg =
            State_initialized.run ~init_args ~capabilities rpc
          in
          State_debug.run ~init_args ~launch_args ~dbg rpc;%lwt
          fst (Lwt.task ())
        with Exit -> Lwt.return_unit );%lwt
      Log.debug (fun m -> m "state_end");
      !cancel ();
      Lwt.return_unit);
  let loop = Debug_rpc.start rpc in
  (cancel := fun () -> Lwt.cancel loop);
  (try%lwt loop with Lwt.Canceled -> Lwt.return_unit);%lwt
  Log.debug (fun m -> m "loop end");
  Lwt.return ()
