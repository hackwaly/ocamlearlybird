open Global
include Lwt_unix

let chdir_lock = Lwt_mutex.create ()

let with_chdir ~cwd fn =
  Lwt_mutex.with_lock chdir_lock (fun () ->
      let%lwt cwd' = Lwt_unix.getcwd () in
      Lwt_unix.chdir cwd;%lwt
      (fn ()) [%finally Lwt_unix.chdir cwd'])
