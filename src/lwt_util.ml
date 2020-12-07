let chdir_lock = Lwt_mutex.create ()

let with_chdir ~cwd fn =
  Lwt_mutex.with_lock chdir_lock (fun () ->
    let%lwt cwd' = Lwt_unix.getcwd () in
    Lwt_unix.chdir cwd;%lwt
    (fn ())[%finally Lwt_unix.chdir cwd']
  )

let loop_read in_chan f =
  let action da =
    let open Lwt_io in
    let break = ref false in
    while%lwt not !break do
      if da.da_ptr < da.da_max then begin
        let content = Lwt_bytes.proxy da.da_buffer da.da_ptr (da.da_max - da.da_ptr) in
        da.da_ptr <- da.da_max;
        let content = Lwt_bytes.to_string content in
        f content
      end else begin
        let%lwt size = da.da_perform () in
        if size = 0 then break := true;
        Lwt.return_unit
      end
    done in
  Lwt_io.direct_access in_chan action

let getstringsockname sock =
  let addr = Lwt_unix.getsockname sock in
  match addr with
  | Unix.ADDR_UNIX addr -> addr
  | Unix.ADDR_INET (addr, port) ->
    Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port
