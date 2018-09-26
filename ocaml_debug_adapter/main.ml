module Session_set = BatSet.Make (struct
    type t = Session.t
    let compare = Pervasives.compare
  end)

let sessions = ref Session_set.empty

let main () =
  let addr = Unix.(ADDR_INET (inet_addr_loopback, 4711)) in
  let%lwt _ = Lwt_io.establish_server_with_client_address addr (
    fun _ (in_chan, out_chan) ->
      let session = Session.create in_chan out_chan in
      sessions := Session_set.add session !sessions;
      Session.start session;%lwt
      sessions := Session_set.remove session !sessions;
      Lwt.return_unit
  ) in
  let wait, _ = Lwt.wait () in
  wait

let () =
  Lwt_main.at_exit (fun () -> 
    Lwt_list.iter_p (fun session -> 
      Session.shutdown session
    ) (Session_set.to_list !sessions)
  );
  Lwt_main.run (main ())
