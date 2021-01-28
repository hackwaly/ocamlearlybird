(**
 * Copyright (C) 2021 Yuxiang Wen
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

type opaque1 = Lwt_unix.file_descr

type opaque2 = Lwt_mutex.t

type io = {
  fd : Lwt_unix.file_descr;
  in_ : Lwt_io.input_channel;
  out : Lwt_io.output_channel;
}

type t = { io : io; mutex : Lwt_mutex.t }

let of_fd fd =
  let close () = Lwt.return () in
  let in_ = Lwt_io.of_fd ~close ~mode:Lwt_io.input fd in
  let out = Lwt_io.of_fd ~close ~mode:Lwt_io.output fd in
  let mutex = Lwt_mutex.create () in
  { io = { fd; in_; out }; mutex }

let close t = Lwt_unix.close t.io.fd

let atomic t f =
  Lwt_mutex.with_lock t.mutex (fun () ->
      f { t with mutex = Lwt_mutex.create () })

let is_busy t = Lwt_mutex.is_locked t.mutex
