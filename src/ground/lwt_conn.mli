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
