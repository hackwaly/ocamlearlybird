open Remote_debugger

type breakpoint_desc = { pc : pc }

type t = {
  bp_by_pc : (pc, breakpoint_desc) Hashtbl.t;
  commit_queue : (pc, unit) Hashtbl.t;
  committed : (pc, unit) Hashtbl.t;
  mutable temporary_breakpoint : pc option;
}

let create () =
  {
    bp_by_pc = Hashtbl.create 0;
    commit_queue = Hashtbl.create 0;
    committed = Hashtbl.create 0;
    temporary_breakpoint = None;
  }

let set t pc =
  let bp = { pc } in
  Hashtbl.replace t.bp_by_pc pc bp;
  Hashtbl.replace t.commit_queue pc ()

let remove t pc =
  Hashtbl.remove t.bp_by_pc pc;
  Hashtbl.replace t.commit_queue pc ()

let is_commited t pc =
  Hashtbl.mem t.committed pc

let check t pc =
  Lwt.return (Hashtbl.mem t.bp_by_pc pc)

let commit t (module Rdbg : Remote_debugger.S) conn =
  let commit_one pc =
    let removed = not (Hashtbl.mem t.bp_by_pc pc) in
    let committed = Hashtbl.mem t.committed pc in
    match (removed, committed) with
    | true, true ->
        Rdbg.reset_instr conn pc;%lwt
        Rdbg.set_event conn pc;%lwt
        Hashtbl.remove t.committed pc |> Lwt.return
    | false, false ->
        Rdbg.reset_instr conn pc;%lwt
        Rdbg.set_breakpoint conn pc;%lwt
        Hashtbl.replace t.committed pc () |> Lwt.return
    | _ -> Lwt.return ()
  in
  t.commit_queue |> Hashtbl.to_seq_keys |> Lwt_util.iter_seq_s commit_one
