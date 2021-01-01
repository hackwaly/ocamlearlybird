include Debugcom_basic

type conn = <
  Debugcom_basic.conn;
  symbols : Symbols.t;
>

let exec_with_frame conn index f =
  Log.debug (fun m -> m "exec_with_frame");%lwt
  assert (index >= 0);
  let rec walk cur (stack_pos, pc) =
    Log.debug (fun m -> m "exec_with_frame cur:%d" cur);%lwt
    let ev = Symbols.find_event conn#symbols pc in
    if cur = index then Lwt.return (Some (stack_pos, pc, ev))
    else
      match%lwt up_frame conn ev.ev.Instruct.ev_stacksize with
      | None -> Lwt.return None
      | Some (stack_pos, pc) -> walk (cur + 1) (stack_pos, pc)
  in
  let%lwt stack_pos, pc = initial_frame conn in
  let%lwt frame = walk 0 (stack_pos, pc) in
  (f frame) [%finally set_frame conn stack_pos]
