type opaque1

type opaque2

type io = {
  fd : opaque1;
  in_ : Lwt_io.input_channel;
  out : Lwt_io.output_channel;
}

type t = { io : io; mutex : opaque2 }

val of_fd : Lwt_unix.file_descr -> t

val close : t -> unit Lwt.t

val atomic : t -> (t -> 'a Lwt.t) -> 'a Lwt.t

val is_busy : t -> bool
