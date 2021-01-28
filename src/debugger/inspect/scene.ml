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

open Ground
open Errors
open Int64ops
open Controller
open Frame
open Instruct
open Debug_types

type t = Controller.t * int64

type obj = Local of Obj.t | Remote of Wire_protocol.remote_value

let from_controller c = (c, c.time)

let _lock_conn (c, time) f =
  Lwt_conn.atomic c.conn (fun conn ->
      if c.dead || time <> c.time then raise Invalid_state;
      (f conn) [%finally [%lwt assert (time = c.time)]])

let _get_frame symbols index (stack_pos, pc) =
  let event = Symbols.find_event_opt symbols pc in
  let loc =
    let open Option in
    let* event = event in
    let* module_ =
      try
        let frag_num, _ = pc in
        let frag = Symbols.find_fragment symbols frag_num in
        Some (Code_fragment.find_module frag event.ev_module)
      with Not_found -> None
    in
    let* source = module_.source in
    let source = source.path in
    if index = 0 then
      let pos = Util.Debug_event.line_column event in
      return { source; pos; end_ = pos }
    else
      let pos = Lexing.Position.line_column event.ev_loc.loc_start in
      let end_ = Lexing.Position.line_column event.ev_loc.loc_end in
      return { source; pos; end_ }
  in
  let typenv =
    Lazy.from_fun (fun () ->
        let event = event |> Option.get in
        let get_search_dirs module_id =
          let frag_num, _ = pc in
          let frag = Symbols.find_fragment symbols frag_num in
          let module_ = Code_fragment.find_module frag module_id in
          module_.search_dirs
        in
        Typenv.from_summary ~get_search_dirs event.ev_typenv event.ev_typsubst)
  in
  let globals =
    Lazy.from_fun (fun () ->
        let frag_num, _ = pc in
        let frag = Symbols.find_fragment symbols frag_num in
        frag.globals)
  in
  { Frame.index; stack_pos; pc; event; loc; typenv; globals }

let top_frame (c, time) =
  if time = _0 || c.dead then Lwt.return None
  else
    _lock_conn (c, time) (fun conn ->
        let%lwt sp_pc = Wire_protocol.get_frame conn in
        Lwt.return (Some (_get_frame c.symbols 0 sp_pc)))

let next_frame (c, time) frame =
  if time = _0 || frame.event |> Option.is_none || c.dead then Lwt.return None
  else
    _lock_conn (c, time) (fun conn ->
        let%lwt sp_pc_opt =
          let%lwt stack_pos0, _ = Wire_protocol.get_frame conn in
          ( Wire_protocol.set_frame conn frame.stack_pos;%lwt
            Wire_protocol.up_frame conn (frame.event |> Option.get).ev_stacksize
          )
            [%finally Wire_protocol.set_frame conn stack_pos0]
        in
        match sp_pc_opt with
        | None -> Lwt.return None
        | Some sp_pc ->
            Lwt.return (Some (_get_frame c.symbols (frame.index + 1) sp_pc)))

let frames t =
  let prev_frame = ref None in
  Lwt_stream.from (fun () ->
      try%lwt
        let%lwt frame =
          match !prev_frame with
          | None -> top_frame t
          | Some frame -> next_frame t frame
        in
        prev_frame := frame;
        Lwt.return frame
      with Invalid_state -> Lwt.return None)

let get_accu (c, time) frame =
  _lock_conn (c, time) (fun conn ->
      let%lwt stack_pos0, _ = Wire_protocol.get_frame conn in
      let%lwt rv =
        ( Wire_protocol.set_frame conn frame.stack_pos;%lwt
          Wire_protocol.get_accu conn )
          [%finally Wire_protocol.set_frame conn stack_pos0]
      in
      Lwt.return (Remote rv))

let get_local (c, time) frame index =
  _lock_conn (c, time) (fun conn ->
      let%lwt stack_pos0, _ = Wire_protocol.get_frame conn in
      let%lwt rv =
        ( Wire_protocol.set_frame conn frame.stack_pos;%lwt
          Wire_protocol.get_local conn
            ((frame.event |> Option.get).ev_stacksize - index) )
          [%finally Wire_protocol.set_frame conn stack_pos0]
      in
      Lwt.return (Remote rv))

let get_environment (c, time) frame index =
  _lock_conn (c, time) (fun conn ->
      let%lwt stack_pos0, _ = Wire_protocol.get_frame conn in
      let%lwt rv =
        ( Wire_protocol.set_frame conn frame.stack_pos;%lwt
          Wire_protocol.get_environment conn index )
          [%finally Wire_protocol.set_frame conn stack_pos0]
      in
      Lwt.return (Remote rv))

let get_global (c, time) index =
  _lock_conn (c, time) (fun conn ->
      let%lwt rv = Wire_protocol.get_global conn index in
      Lwt.return (Remote rv))

let is_block rv =
  match rv with
  | Local v -> Obj.is_block v
  | Remote rv -> Obj.is_block (Array.unsafe_get (Obj.magic rv : Obj.t array) 0)

let get_field (c, time) rv index =
  if not (is_block rv) then failwith "get_field";
  match rv with
  | Local v -> Lwt.return (Local (Obj.field v index))
  | Remote rv ->
      _lock_conn (c, time) (fun conn ->
          match%lwt Wire_protocol.get_field conn rv index with
          | rv' -> Lwt.return (Remote rv')
          | exception Wire_protocol.Float_field v ->
              Lwt.return (Local (Obj.repr v)))

let get_tag (c, time) rv =
  match rv with
  | Local v -> Lwt.return (Obj.tag v)
  | Remote rv ->
      _lock_conn (c, time) (fun conn ->
          let%lwt hdr = Wire_protocol.get_header conn rv in
          let tag = hdr land 0xff in
          Lwt.return tag)

let get_size (c, time) rv =
  match rv with
  | Local v -> Lwt.return (Obj.size v)
  | Remote rv ->
      _lock_conn (c, time) (fun conn ->
          let%lwt hdr = Wire_protocol.get_header conn rv in
          let size =
            if hdr land 0xff = Obj.double_array_tag && Sys.word_size = 32 then
              hdr lsr 11
            else hdr lsr 10
          in
          Lwt.return size)

let marshal_obj (c, time) rv =
  match rv with
  | Local v -> Lwt.return (Obj.magic v)
  | Remote rv ->
      _lock_conn (c, time) (fun conn -> Wire_protocol.marshal_obj conn rv)

let get_closure_code (c, time) rv =
  match rv with
  | Local _ -> raise (Invalid_argument "rv")
  | Remote rv ->
      _lock_conn (c, time) (fun conn ->
          let%lwt pc = Wire_protocol.get_closure_code conn rv in
          let event = Symbols.find_event_opt c.symbols pc in
          Lwt.return (event |> Option.map (fun event -> (fst pc, event))))
