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
