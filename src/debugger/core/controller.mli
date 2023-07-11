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
  mutable unstarted : bool;
}

val root :
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
  ?trap_barrier:Sp.t ->
  ?temporary_breakpoint:pc ->
  t ->
  int64 ->
  ( [ `Event
    | `Yield_stop of int
    | `Breakpoint
    | `Exited
    | `Trap_barrier
    | `Uncaught_exc ]
  * int64 * (Sp.t * (int * int)) option )
  Lwt.t
