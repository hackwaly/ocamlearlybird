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
open Debug_types

type t = {
  parent : t option;
  pid : int;
  conn : Lwt_conn.t;
  symbols : Symbols.t;
  mutable breakpoints : Set.Make(Ordered_type.Make_tuple2(Int)(Int)).t;
  mutable debug_modules : Set.Make(Ordered_type.Make_tuple2(Int)(String)).t;
  mutable time : int64;
  mutable dead : bool;
}

val root :
  ?source_resolver:(string -> string list -> string option Lwt.t) ->
  ?debug_filter:(string -> bool) ->
  Lwt_unix.file_descr ->
  string ->
  t Lwt.t

val fork : t -> Lwt_unix.file_descr -> t Lwt.t

val is_busy : t -> bool

val set_follow_fork_mode : t -> [`Fork_parent | `Fork_child] -> unit Lwt.t

val set_breakpoint : t -> pc -> unit Lwt.t

val remove_breakpoint : t -> pc -> unit Lwt.t

val stop : ?gracefully:bool -> t -> unit Lwt.t

val execute :
  ?yield_steps:int ->
  ?on_yield:(unit -> [ `Continue | `Stop of int ] Lwt.t) ->
  ?trap_barrier:int ->
  ?temporary_breakpoint:pc ->
  t ->
  int64 ->
  ( [ `Event
    | `Yield_stop of int
    | `Breakpoint
    | `Exited
    | `Trap_barrier
    | `Uncaught_exc ]
  * int64 * (int * (int * int)) option )
  Lwt.t
