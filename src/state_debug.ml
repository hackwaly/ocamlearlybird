let run ~launch_args ~terminate ~agent rpc =
  Lwt.join [
    Breakpoints.run ~launch_args ~terminate ~agent rpc;
    Inspect.run ~launch_args ~terminate ~agent rpc;
    Time_travel.run ~launch_args ~terminate ~agent rpc;
    Lifecycle.run ~launch_args ~terminate ~agent rpc;
  ]
