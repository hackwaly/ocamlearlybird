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
open Lwt_conn

type pc = int * int

let _read_pc conn =
  let%lwt frag_num = Lwt_io.BE.read_int conn.io.in_ in
  let%lwt pos = Lwt_io.BE.read_int conn.io.in_ in
  Lwt.return (frag_num, pos)

let _write_pc conn (frag_num, pos) =
  Lwt_io.BE.write_int conn.io.out frag_num;%lwt
  Lwt_io.BE.write_int conn.io.out pos

let set_fork_mode conn mode =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'K';%lwt
      Lwt_io.BE.write_int conn.io.out
        (match mode with `Fork_child -> 0 | `Fork_parent -> 1))

let set_event conn pc =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'e';%lwt
      _write_pc conn pc)

let set_breakpoint conn pc =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'B';%lwt
      _write_pc conn pc)

let reset_instr conn pc =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'i';%lwt
      _write_pc conn pc)

let set_trap_barrier conn pos =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'b';%lwt
      Lwt_io.BE.write_int conn.io.out pos)

let checkpoint conn =
  Lwt_conn.atomic conn (fun conn ->
      if Sys.win32 then raise Operation_system_unsupported
      else Lwt_io.write_char conn.io.out 'c';%lwt
      let%lwt pid = Lwt_io.BE.read_int conn.io.in_ in
      if pid = -1 then assert false else Lwt.return pid)

type execution_summary =
  [ `Event
  | `Breakpoint
  | `Exited
  | `Trap_barrier
  | `Uncaught_exc
  | `Debug_info of
    int Ident.Map.t * (Instruct.debug_event list * string list) list
  | `Code_loaded of int
  | `Code_unloaded of int ]

let go conn steps =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'g';%lwt
      Lwt_io.BE.write_int conn.io.out steps;%lwt
      let%lwt summary =
        match%lwt Lwt_io.read_char conn.io.in_ with
        | 'e' -> Lwt.return `Event
        | 'b' -> Lwt.return `Breakpoint
        | 'x' -> Lwt.return `Exited
        | 's' -> Lwt.return `Trap_barrier
        | 'u' -> Lwt.return `Uncaught_exc
        | 'D' ->
            let%lwt (evls : Instruct.debug_event list array) =
              Lwt_io.read_value conn.io.in_
            in
            let evls =
              evls |> Array.to_seq
              |> Seq.map (fun evl -> (evl, []))
              |> List.of_seq
            in
            Lwt.return (`Debug_info (Ident.Map.empty, evls))
        | 'L' ->
            let%lwt frag_num = Lwt_io.BE.read_int conn.io.in_ in
            Lwt.return (`Code_loaded frag_num)
        | 'U' ->
            let%lwt frag_num = Lwt_io.BE.read_int conn.io.in_ in
            Lwt.return (`Code_unloaded frag_num)
        | _ -> assert false
      in
      let%lwt executed_steps = Lwt_io.BE.read_int conn.io.in_ in
      let%lwt sp = Lwt_io.BE.read_int conn.io.in_ in
      let%lwt pc = _read_pc conn in
      Lwt.return
        ( executed_steps,
          summary,
          match (sp, pc) with 0, (-1, 0) -> None | _ -> Some (sp, pc) ))

let wait conn =
  Lwt_conn.atomic conn (fun conn -> Lwt_io.write_char conn.io.out 'w')

let stop conn =
  Lwt_conn.atomic conn (fun conn -> Lwt_io.write_char conn.io.out 's')

let initial_frame conn =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out '0';%lwt
      let%lwt stack_pos = Lwt_io.BE.read_int conn.io.in_ in
      let%lwt pc = _read_pc conn in
      Lwt.return (stack_pos, pc))

let get_frame conn =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'f';%lwt
      let%lwt stack_pos = Lwt_io.BE.read_int conn.io.in_ in
      let%lwt pc = _read_pc conn in
      Lwt.return (stack_pos, pc))

let set_frame conn stack_pos =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'S';%lwt
      Lwt_io.BE.write_int conn.io.out stack_pos)

let up_frame conn stacksize =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'U';%lwt
      Lwt_io.BE.write_int conn.io.out stacksize;%lwt
      let%lwt stack_pos = Lwt_io.BE.read_int conn.io.in_ in
      let%lwt res =
        if stack_pos = -1 then Lwt.return None
        else
          let%lwt pc = _read_pc conn in
          Lwt.return (Some (stack_pos, pc))
      in
      Lwt.return res)

type remote_value = string

let _read_remote_value conn =
  Lwt_io.read_string_exactly conn.io.in_ (Sys.word_size / 8)

let _write_remote_value conn rv = Lwt_io.write conn.io.out rv

let get_local conn index =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'L';%lwt
      Lwt_io.BE.write_int conn.io.out index;%lwt
      let%lwt rv = _read_remote_value conn in
      Lwt.return rv)

let get_environment conn index =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'E';%lwt
      Lwt_io.BE.write_int conn.io.out index;%lwt
      let%lwt rv = _read_remote_value conn in
      Lwt.return rv)

let get_global conn index =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'G';%lwt
      Lwt_io.BE.write_int conn.io.out index;%lwt
      let%lwt rv = _read_remote_value conn in
      Lwt.return rv)

let get_accu conn =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'A';%lwt
      let%lwt rv = _read_remote_value conn in
      Lwt.return rv)

let get_header conn rv =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'H';%lwt
      _write_remote_value conn rv;%lwt
      let%lwt hdr = Lwt_io.BE.read_int conn.io.in_ in
      Lwt.return hdr)

exception Float_field of float

let get_field conn rv index =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'F';%lwt
      _write_remote_value conn rv;%lwt
      Lwt_io.BE.write_int conn.io.out index;%lwt
      let%lwt res =
        match%lwt Lwt_io.read_char conn.io.in_ with
        | '\000' ->
            let%lwt rv = _read_remote_value conn in
            Lwt.return rv
        | '\001' ->
            let%lwt v = Lwt_io.read_float64 conn.io.in_ in
            Lwt.fail (Float_field v)
        | _ -> assert false
      in
      Lwt.return res)

let marshal_obj conn rv =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'M';%lwt
      _write_remote_value conn rv;%lwt
      let%lwt v = Lwt_io.read_value conn.io.in_ in
      Lwt.return v)

let get_closure_code conn rv =
  Lwt_conn.atomic conn (fun conn ->
      Lwt_io.write_char conn.io.out 'C';%lwt
      _write_remote_value conn rv;%lwt
      let%lwt pc = _read_pc conn in
      Lwt.return pc)
