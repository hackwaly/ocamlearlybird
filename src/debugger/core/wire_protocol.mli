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

type pc = int * int

type execution_summary = [
  | `Event
  | `Breakpoint
  | `Exited
  | `Trap_barrier
  | `Uncaught_exc
  | `Debug_info of int Ident.Map.t * (Instruct.debug_event list * string list) list
  | `Code_loaded of int
  | `Code_unloaded of int
]

exception Float_field of float

type remote_value

val set_fork_mode : Lwt_conn.t -> [`Fork_child | `Fork_parent] -> unit Lwt.t

val set_event : Lwt_conn.t -> pc -> unit Lwt.t

val set_breakpoint : Lwt_conn.t -> pc -> unit Lwt.t

val reset_instr : Lwt_conn.t -> pc -> unit Lwt.t

val set_trap_barrier : Lwt_conn.t -> int -> unit Lwt.t

val checkpoint : Lwt_conn.t -> int Lwt.t

val go :
  Lwt_conn.t -> int -> (int * execution_summary * (int * pc) option) Lwt.t

val stop : Lwt_conn.t -> unit Lwt.t

val wait : Lwt_conn.t -> unit Lwt.t

val initial_frame : Lwt_conn.t -> (int * pc) Lwt.t

val get_frame : Lwt_conn.t -> (int * pc) Lwt.t

val set_frame : Lwt_conn.t -> int -> unit Lwt.t

val up_frame : Lwt_conn.t -> int -> (int * pc) option Lwt.t

val get_local : Lwt_conn.t -> int -> remote_value Lwt.t

val get_environment : Lwt_conn.t -> int -> remote_value Lwt.t

val get_global : Lwt_conn.t -> int -> remote_value Lwt.t

val get_accu : Lwt_conn.t -> remote_value Lwt.t

val get_header : Lwt_conn.t -> remote_value -> int Lwt.t

val get_field : Lwt_conn.t -> remote_value -> int -> remote_value Lwt.t

val marshal_obj : Lwt_conn.t -> remote_value -> 'a Lwt.t

val get_closure_code : Lwt_conn.t -> remote_value -> pc Lwt.t
