let src = Logs.Src.create "earlybird.Session"

module Log = (val Logs_lwt.src_log src : Logs_lwt.LOG)

let start rpc =
  let cancel = ref (fun () -> ()) in
  Lwt.async (fun () ->
      ( try%lwt
          Log.debug (fun m -> m "state_uninitialized") ;%lwt
          let%lwt init_args, capabilities = State_uninitialized.run rpc in
          Log.debug (fun m -> m "state_initialized") ;%lwt
          let%lwt launch_args, debug, terminate =
            State_initialized.run ~init_args ~capabilities rpc
          in
          ( match debug with
          | No_debug ->
              Log.debug (fun m -> m "state_no_debug") ;%lwt
              State_no_debug.run ~launch_args ~terminate rpc
          | Debug agent ->
              Log.debug (fun m -> m "state_debug") ;%lwt
              State_debug.run ~launch_args ~terminate ~agent rpc ) ;%lwt
          fst (Lwt.task ())
        with Exit -> Lwt.return_unit ) ;%lwt
      Log.debug (fun m -> m "state_end") ;%lwt
      !cancel () ;
      Lwt.return_unit) ;
  let loop = Debug_rpc.start rpc in
  (cancel := fun () -> Lwt.cancel loop) ;
  (try%lwt loop with Lwt.Canceled -> Lwt.return_unit) ;%lwt
  Log.debug (fun m -> m "loop end")
