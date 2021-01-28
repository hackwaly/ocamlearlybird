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

open Global
include Lwt_io

let read_string_exactly ic count =
  let buf = Bytes.create count in
  Lwt_io.read_into_exactly ic buf 0 count;%lwt
  Lwt.return (Bytes.to_string buf)

let loop_read in_chan f =
  let action da =
    let open Lwt_io in
    let break = ref false in
    while%lwt not !break do
      if da.da_ptr < da.da_max then (
        let content =
          Lwt_bytes.proxy da.da_buffer da.da_ptr (da.da_max - da.da_ptr)
        in
        da.da_ptr <- da.da_max;
        let content = Lwt_bytes.to_string content in
        f content )
      else
        let%lwt size = da.da_perform () in
        if size = 0 then break := true;
        Lwt.return_unit
    done
  in
  Lwt_io.direct_access in_chan action
