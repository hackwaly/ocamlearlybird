open Debug_protocol_ex

let run rpc =
  let (promise, resolve) = Lwt.task () in
  let prevent_reenter () =
    Debug_rpc.remove_command_handler rpc (module Initialize_command);
  in
  Debug_rpc.set_command_handler rpc (module Initialize_command) (fun arg ->
    prevent_reenter ();
    let caps = Capabilities.(make ()) in
    Lwt.wakeup_later resolve (arg, caps);
    Lwt.return caps
  );
  promise
