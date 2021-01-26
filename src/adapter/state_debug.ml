let run ~launch_args ~dbg rpc =
  Lwt.join
    [
      Lifecycle.run ~launch_args ~dbg rpc;
      Inspect.run ~launch_args ~dbg rpc;
      Breakpoints.run ~launch_args ~dbg rpc;
      Time_travel.run ~launch_args ~dbg rpc;
    ]
