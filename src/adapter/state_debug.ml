let run ~init_args ~launch_args ~dbg rpc =
  Lwt.join
    [
      Lifecycle.run ~init_args ~launch_args ~dbg rpc;
      Inspect.run ~init_args ~launch_args ~dbg rpc;
      Breakpoints.run ~init_args ~launch_args ~dbg rpc;
      Time_travel.run ~init_args ~launch_args ~dbg rpc;
    ]
