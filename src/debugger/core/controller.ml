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
open Instruct
module StringMap_ = Map.Make (String)
module PcSet_ = Set.Make (Ordered_type.Make_tuple2 (Int) (Int))
module FragModuleIdSet_ = Set.Make (Ordered_type.Make_tuple2 (Int) (String))

type t = {
  parent : t option;
  pid : int;
  conn : Lwt_conn.t;
  symbols : Symbols.t;
  mutable breakpoints : PcSet_.t;
  mutable debug_modules : FragModuleIdSet_.t;
  mutable time : int64;
  mutable dead : bool;
  mutable unstarted : bool;
}

type execution_summary =
  [ `Event
  | `Breakpoint
  | `Exited
  | `Trap_barrier
  | `Uncaught_exc
  | `Yield_stop of int ]

let _set_frag_events symbols conn frag =
  let debug_modules =
    frag |> Code_fragment.to_source_modules_seq
    |> Seq.filter (fun it ->
           symbols.Symbols.debug_filter
             (it.Code_module.source |> Option.get).path)
  in
  debug_modules
  |> Seq.flat_map (fun (module_ : Code_module.t) ->
         module_.events |> Array.to_seq
         |> Seq.map (fun it -> (module_.frag, it.ev_pos)))
  |> Lwt_seq.iter_s (Wire_protocol.set_event conn);%lwt
  Lwt.return
    (debug_modules
    |> Seq.map (fun (it : Code_module.t) -> (it.frag, it.module_id))
    |> FragModuleIdSet_.of_seq)

let root ?debug_filter debug_sock symbols_file =
  let%lwt fd, _ = Lwt_unix.accept debug_sock in
  let conn = Lwt_conn.of_fd fd in
  let%lwt neg1 = Lwt_io.BE.read_int conn.io.in_ in
  assert (neg1 = -1);
  let%lwt pid = Lwt_io.BE.read_int conn.io.in_ in
  let%lwt debug_info = Bytecode.load_debuginfo symbols_file in
  let frag = Code_fragment.make 0 debug_info in
  let symbols = Symbols.create ?debug_filter () in
  Symbols.add_fragment symbols frag;%lwt
  let%lwt debug_modules = _set_frag_events symbols conn frag in
  Lwt.return
    {
      parent = None;
      pid;
      conn;
      symbols;
      breakpoints = PcSet_.empty;
      debug_modules;
      time = _0;
      dead = false;
      unstarted = true;
    }

let fork t debug_sock =
  if t.dead then raise Invalid_state;
  let%lwt pid = Wire_protocol.checkpoint t.conn in
  let rec wait_conn () =
    let%lwt fd, _ = Lwt_unix.accept debug_sock in
    let conn = Lwt_conn.of_fd fd in
    let%lwt pid' = Lwt_io.BE.read_int conn.io.in_ in
    if pid' = pid then Lwt.return conn
    else (
      Lwt_unix.close fd;%lwt
      wait_conn ())
  in
  let%lwt conn = wait_conn () in
  Lwt.return
    {
      parent = Some t;
      pid;
      conn;
      symbols = Symbols.dup t.symbols;
      breakpoints = t.breakpoints;
      debug_modules = t.debug_modules;
      time = t.time;
      dead = false;
      unstarted = t.unstarted;
    }

let is_busy t = Lwt_conn.is_busy t.conn

let set_follow_fork_mode t mode = Wire_protocol.set_fork_mode t.conn mode

(* TODO: Check symbols.source_modules *)
let set_breakpoint t pc =
  Wire_protocol.set_event t.conn pc;%lwt
  Wire_protocol.set_breakpoint t.conn pc;%lwt
  t.breakpoints <- PcSet_.add pc t.breakpoints;
  Lwt.return ()

let remove_breakpoint t pc =
  Wire_protocol.reset_instr t.conn pc;%lwt
  Wire_protocol.set_event t.conn pc;%lwt
  t.breakpoints <- PcSet_.remove pc t.breakpoints;
  Lwt.return ()

let stop ?(gracefully = false) t =
  if t.dead then Lwt.return ()
  else (
    if gracefully then Wire_protocol.stop t.conn
    else (
      Unix.kill t.pid 9;
      Lwt.return ());%lwt
    t.dead <- true;
    let () =
      match t.parent with
      | None -> ()
      | Some parent -> Lwt.async (fun () -> Wire_protocol.wait parent.conn)
    in
    Lwt.return ())

let execute ?(yield_steps = Int.max_int)
    ?(on_yield = fun () -> Lwt.return `Continue) ?trap_barrier
    ?temporary_breakpoint t steps =
  let rec exec_smallint steps =
    let int_step = min (min steps max_small_int |> Int64.to_int) yield_steps in
    let%lwt irem_steps, summary, sp_pc = Wire_protocol.go t.conn int_step in
    t.unstarted <- false;
    let executed_steps = Int64.of_int (int_step - irem_steps) in
    t.time <- t.time ++ executed_steps;
    let remaining_steps = steps -- executed_steps in
    if summary = `Event && remaining_steps > _0 then
      match%lwt on_yield () with
      | `Continue -> exec_smallint remaining_steps
      | `Stop tag -> Lwt.return (`Yield_stop tag, remaining_steps, sp_pc)
    else
      Lwt.return
        ( (summary :> [ Wire_protocol.execution_summary | `Yield_stop of int ]),
          remaining_steps,
          sp_pc )
  in
  let last_debug_info = ref None in
  let rec exec_dynlink steps =
    let%lwt summary, remaining_steps, sp_pc = exec_smallint steps in
    match summary with
    | `Debug_info debug_info ->
        last_debug_info := Some debug_info;
        exec_dynlink remaining_steps
    | `Code_loaded frag_num ->
        let debug_info = !last_debug_info |> Option.get in
        last_debug_info := None;
        let frag = Code_fragment.make frag_num debug_info in
        Symbols.add_fragment t.symbols frag;%lwt
        let%lwt debug_modules = _set_frag_events t.symbols t.conn frag in
        t.debug_modules <- FragModuleIdSet_.union t.debug_modules debug_modules;
        exec_dynlink remaining_steps
    | `Code_unloaded frag_num ->
        Symbols.remove_fragment t.symbols frag_num;
        t.debug_modules <-
          t.debug_modules
          |> FragModuleIdSet_.filter (fun (frag_num', _) ->
                 frag_num' <> frag_num);
        exec_dynlink remaining_steps
    | #execution_summary as summary ->
        Lwt.return (summary, remaining_steps, sp_pc)
  in
  let run () = exec_dynlink steps in
  let run =
    match temporary_breakpoint with
    | None -> run
    | Some pc ->
        fun () ->
          Wire_protocol.set_breakpoint t.conn pc;%lwt
          let%lwt r = run () in
          if not (t.breakpoints |> PcSet_.mem pc) then (
            Wire_protocol.reset_instr t.conn pc;%lwt
            Wire_protocol.set_event t.conn pc)
          else Lwt.return ();%lwt
          Lwt.return r
  in
  let run () =
    let%lwt () =
      match trap_barrier with
      | None -> Lwt.return ()
      | Some trap_barrier -> Wire_protocol.set_trap_barrier t.conn trap_barrier
    in
    let%lwt summary, remaining_steps, sp_pc = run () in
    Wire_protocol.set_trap_barrier t.conn 0;%lwt
    if summary = `Trap_barrier then
      let rec stop_on_event () =
        let%lwt summary', remaining_steps', sp_pc' = exec_dynlink _1 in
        let remaining_steps = remaining_steps ++ (_1 -- remaining_steps') in
        match summary' with
        | `Trap_barrier -> stop_on_event ()
        | `Event | `Breakpoint ->
            Lwt.return (`Trap_barrier, remaining_steps, sp_pc')
        | `Exited | `Uncaught_exc | `Yield_stop _ ->
            Lwt.return (summary', remaining_steps, sp_pc')
      in
      stop_on_event ()
    else Lwt.return (summary, remaining_steps, sp_pc)
  in
  let%lwt summary, remaining_steps, sp_pc = run () in
  if summary = `Exited || summary = `Uncaught_exc then stop ~gracefully:true t
  else Lwt.return ();%lwt
  Lwt.return (summary, remaining_steps, sp_pc)
