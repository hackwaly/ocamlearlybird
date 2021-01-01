type pc = Pc.t

type breakpoint_desc = { pc : pc }

type t = {
  bp_by_pc : (pc, breakpoint_desc) Hashtbl.t;
  commit_queue : (pc, unit) Hashtbl.t;
  committed : (pc, unit) Hashtbl.t;
}

let create () =
  {
    bp_by_pc = Hashtbl.create 0;
    commit_queue = Hashtbl.create 0;
    committed = Hashtbl.create 0;
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

let commit t set clear =
	let to_set =
  	t.commit_queue
    |> Hashtbl.to_seq_keys
    |> Seq.filter (fun pc ->
      Hashtbl.mem t.bp_by_pc pc &&
      not (Hashtbl.mem t.committed pc))
  in
  let to_clear =
  	t.commit_queue
    |> Hashtbl.to_seq_keys
    |> Seq.filter (fun pc ->
      not (Hashtbl.mem t.bp_by_pc pc) &&
      Hashtbl.mem t.committed pc)
  in
  to_set |> Lwt_util.iter_seq_s set;%lwt
  to_clear |> Lwt_util.iter_seq_s clear;%lwt
  Hashtbl.reset t.committed;
  Lwt.return ()
