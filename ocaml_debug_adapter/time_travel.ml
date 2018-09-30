open Debug_adapter_protocol
open Debug_protocol
open Debug_protocol_ex
open Signatures

module Make (Args : sig
    val rpc : Rpc.t
    val replace_agent : (module AGENT) -> unit
    val init_args : Initialize_command.Request.Arguments.t
    val caps : Capabilities.t
    val launch_args : Launch_command.Request.Arguments.t
    val proc : Agent_launched.launched_process
    val symbols : Symbols.t
    val conn : Debug_conn.t
    val pid : int
    val state : [`At_entry | `Running | `Stopped | `Exited] ref
    val report : Debug_conn.report * [`Step | `No_guide] -> unit Lwt.t
    val has_breakpoint_at : Debug_conn.report -> bool
    val get_frames : int option -> (int * Instruct.debug_event) array Lwt.t
  end) = struct

  include Args

  let get_parent_frame () =
    let%lwt frames = get_frames (Some 2) in
    if Array.length frames < 2
    then Lwt.return_none
    else Lwt.return_some (Array.get frames 1)

  let run () =
    let rec internal_run () =
      let%lwt report = Debug_conn.go conn max_int in
      if report.rep_type = Event then internal_run ()
      else Lwt.return report
    in
    state := `Running;
    let%lwt rep =
      try%lwt internal_run ()
      with End_of_file -> Lwt.fail Exit
    in
    report (rep, `No_guide)

  let step () =
    let%lwt rep = Debug_conn.go conn 1 in
    let rec stop_on_event (rep : Debug_conn.report) =
      match rep.rep_type with
      | Exited | Uncaught_exc -> Lwt.return (rep, `No_guide)
      | Breakpoint -> (
          match Symbols.event_at_pc symbols rep.rep_program_pointer with
          | exception Not_found ->
            let%lwt rep = Debug_conn.go conn 1 in
            stop_on_event rep
          | _ -> Lwt.return (rep, `No_guide)
        )
      | Trap_barrier ->
        let%lwt rep = Debug_conn.go conn 1 in
        stop_on_event rep
      | Event -> Lwt.return (rep, `Step)
    in
    stop_on_event rep

  let step_out (stack_pos, (ev : Instruct.debug_event)) =
    let at_return_point (rep : Debug_conn.report) =
      rep.rep_program_pointer = ev.ev_pos && rep.rep_stack_pointer = stack_pos
    in
    Debug_conn.exec_with_trap_barrier conn stack_pos (fun conn ->
      Debug_conn.exec_with_temporary_breakpoint conn ev.ev_pos (fun conn ->
        let rec loop () =
          let%lwt rep = Debug_conn.go conn max_int in
          match rep.rep_type with
          | Exited  | Uncaught_exc -> Lwt.return (rep, `No_guide)
          | Breakpoint when has_breakpoint_at rep -> Lwt.return (rep, `No_guide)
          | Breakpoint | Trap_barrier when at_return_point rep -> Lwt.return (rep, `Step)
          | _ -> loop ()
        in
        loop ()
      )
    )

  let next () =
    let%lwt prev_stack = get_frames None in
    let%lwt rep = step () in
    let%lwt next_stack = get_frames None in
    if Array.length next_stack > Array.length prev_stack then (
      BatArray.rev_in_place prev_stack;
      BatArray.rev_in_place next_stack;
      let frame = next_stack.(Array.length prev_stack - 1) in
      step_out frame
    ) else Lwt.return rep


  let configuration_done_command _ =
    if%lwt Lwt.return launch_args.stop_on_entry then (
      Rpc.emit_event rpc (module Stopped_event) {
        reason = "entry";
        description = None;
        thread_id = None;
        preserve_focus_hint = false;
        text = None;
        all_threads_stopped = true;
      }
    ) else (
      Lwt_util.async run;
      Lwt.return_unit
    );%lwt
    Lwt.return_ok ()

  let pause_command _ = Lwt.return_error ("Not supported", None)

  let next_command _ =
    Lwt_util.async (fun () ->
      state := `Running;
      let%lwt rep = next () in
      report rep
    );
    Lwt.return_ok ()

  let continue_command _ =
    Lwt_util.async run;
    Lwt.return_ok Continue_command.Response.Body.({
      all_threads_continued = true;
    })

  let step_in_command _ =
    Lwt_util.async (fun () ->
      state := `Running;
      let%lwt rep = step () in
      report rep
    );
    Lwt.return_ok ()

  let step_in_targets_command _ = Lwt.return_error ("Not supported", None)

  let step_out_command _ =
    let%lwt frame = get_parent_frame () in
    match frame with
    | Some frame -> (
        Lwt_util.async (fun () ->
          state := `Running;
          let%lwt rep = step_out frame in
          report rep
        );
        Lwt.return_ok ()
      )
    | None ->
      Lwt.return_error ("Already at outermost", None)

  let step_back_command _ = Lwt.return_error ("Not supported", None)

  let reverse_continue_command _ = Lwt.return_error ("Not supported", None)
end
