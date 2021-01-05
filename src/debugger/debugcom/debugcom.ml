include Debugcom_basic

type conn = < Debugcom_basic.conn ; symbols : Symbols.t >

let go conn n =
  let%lwt report = Debugcom_basic.go conn n in
  Log.debug (fun m -> m "%s" (show_report report));%lwt
  Lwt.return report

let initial_frame conn =
  let%lwt stack_pos, pc = Debugcom_basic.initial_frame conn in
  let frame =
    { Frame.index = 0; stack_pos; event = Symbols.find_event conn#symbols pc }
  in
  Lwt.return frame

let up_frame conn frame =
  match%lwt up_frame conn frame.Frame.event.ev.Instruct.ev_stacksize with
  | None -> Lwt.return None
  | Some (stack_pos, pc) ->
      let frame' =
        {
          Frame.index = frame.index + 1;
          stack_pos;
          event = Symbols.find_event conn#symbols pc;
        }
      in
      Lwt.return (Some frame')

let set_frame conn frame =
  Debugcom_basic.set_frame conn frame.Frame.stack_pos

let exec_with_frame conn index f =
  assert (index >= 0);
  let rec walk cur frame =
    if cur = index then Lwt.return (Some frame)
    else
      match%lwt up_frame conn frame with
      | None -> Lwt.return None
      | Some frame -> walk (cur + 1) frame
  in
  let%lwt frame0 = initial_frame conn in
  let%lwt frame = walk 0 frame0 in
  (f frame) [%finally set_frame conn frame0]
