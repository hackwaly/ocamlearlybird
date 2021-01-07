include Debugcom_types

type conn =
  < Debugcom_basic.conn
  ; symbols : Symbols.t
  ; lock : 'a. (conn -> 'a Lwt.t) -> 'a Lwt.t >

type remote_value = Local of Obj.t | Remote of Debugcom_basic.remote_value

let make_conn protocol_version symbols fd =
  let module L = struct
    type t = {
      protocol_version : protocol_version;
      io_in : Lwt_io.input_channel;
      io_out : Lwt_io.output_channel;
      symbols : Symbols.t;
      mutex : Lwt_mutex.t option;
    }

    class c r =
      object (self)
        method io_in = r.io_in

        method io_out = r.io_out

        method protocol_version = r.protocol_version

        method symbols = r.symbols

        method lock : 'a. (conn -> 'a Lwt.t) -> 'a Lwt.t =
          fun f ->
            match r.mutex with
            | Some mutex ->
                Lwt_mutex.lock mutex;%lwt
                (f (new c { r with mutex = None }))
                  [%finally
                    Lwt_mutex.unlock mutex;
                    Lwt.return ()]
            | None -> f (self :> conn)
      end
  end in
  let r =
    {
      L.protocol_version;
      io_in = Lwt_io.(of_fd ~mode:input fd);
      io_out = Lwt_io.(of_fd ~mode:output fd);
      symbols;
      mutex = None;
    }
  in
  (new L.c r :> conn)

let get_pid (conn : conn) = conn#lock (fun conn -> Debugcom_basic.get_pid conn)

let set_event (conn : conn) pc =
  conn#lock (fun conn -> Debugcom_basic.set_event conn pc)

let set_breakpoint (conn : conn) pc =
  conn#lock (fun conn -> Debugcom_basic.set_breakpoint conn pc)

let reset_instr (conn : conn) pc =
  conn#lock (fun conn -> Debugcom_basic.reset_instr conn pc)

let checkpoint (conn : conn) =
  conn#lock (fun conn -> Debugcom_basic.checkpoint conn)

let stop (conn : conn) = conn#lock (fun conn -> Debugcom_basic.stop conn)

let wait (conn : conn) = conn#lock (fun conn -> Debugcom_basic.wait conn)

let set_trap_barrier (conn : conn) pos =
  conn#lock (fun conn -> Debugcom_basic.set_trap_barrier conn pos)

let get_local (conn : conn) index =
  conn#lock (fun conn ->
      let%lwt rv = Debugcom_basic.get_local conn index in
      Lwt.return (Remote rv))

let get_environment (conn : conn) index =
  conn#lock (fun conn ->
      let%lwt rv = Debugcom_basic.get_environment conn index in
      Lwt.return (Remote rv))

let get_global (conn : conn) index =
  conn#lock (fun conn ->
      let%lwt rv = Debugcom_basic.get_global conn index in
      Lwt.return (Remote rv))

let get_accu (conn : conn) =
  conn#lock (fun conn ->
      let%lwt rv = Debugcom_basic.get_accu conn in
      Lwt.return (Remote rv))

let get_field (conn : conn) rv index =
  match rv with
  | Local v ->
      let v = Obj.field v index in
      Lwt.return (Local v)
  | Remote rv ->
      conn#lock (fun conn ->
          match%lwt Debugcom_basic.get_field conn rv index with
          | rv -> Lwt.return (Remote rv)
          | exception Debugcom_basic.Float_field v ->
              Lwt.return (Local (Obj.repr v)))

let marshal_obj (conn : conn) rv =
  match rv with
  | Local v -> Lwt.return (Obj.magic v)
  | Remote rv -> conn#lock (fun conn -> Debugcom_basic.marshal_obj conn rv)

let get_closure_code (conn : conn) rv =
  match rv with
  | Local _ -> [%lwt assert false]
  | Remote rv -> conn#lock (fun conn -> Debugcom_basic.get_closure_code conn rv)

let set_fork_mode (conn : conn) mode =
  conn#lock (fun conn -> Debugcom_basic.set_fork_mode conn mode)

let get_tag (conn : conn) rv =
  match rv with
  | Local v -> Lwt.return (Obj.tag v)
  | Remote rv ->
      conn#lock (fun conn ->
          let%lwt hdr = Debugcom_basic.get_header conn rv in
          let tag = hdr land 0xff in
          Lwt.return tag)

let get_size (conn : conn) rv =
  match rv with
  | Local v -> Lwt.return (Obj.size v)
  | Remote rv ->
      conn#lock (fun conn ->
          let%lwt hdr = Debugcom_basic.get_header conn rv in
          let size =
            if hdr land 0xff = Obj.double_array_tag && Sys.word_size = 32 then
              hdr lsr 11
            else hdr lsr 10
          in
          Lwt.return size)

let is_block rv =
  match rv with
  | Local v -> Obj.is_block v
  | Remote rv -> Obj.is_block (Array.unsafe_get (Obj.magic rv : Obj.t array) 0)

let go (conn : conn) n =
  conn#lock (fun conn ->
      let%lwt report = Debugcom_basic.go conn n in
      ( match Symbols.find_event conn#symbols report.rep_program_pointer with
      | event ->
          Log.debug (fun m ->
              m "Report: %s\nEvent: %s" (show_report report)
                (Debuginfo.show_event event))
      | exception Not_found -> Lwt.return () );%lwt
      Lwt.return report)

let initial_frame (conn : conn) =
  conn#lock (fun conn ->
      let%lwt stack_pos, pc = Debugcom_basic.initial_frame conn in
      let frame =
        {
          Frame.index = 0;
          stack_pos;
          event = Symbols.find_event conn#symbols pc;
        }
      in
      Lwt.return frame)

let up_frame (conn : conn) frame =
  conn#lock (fun conn ->
      let%lwt stack_pos0, _ = Debugcom_basic.get_frame conn in
      Debugcom_basic.set_frame conn frame.Frame.stack_pos;%lwt
      ( match%lwt
          Debugcom_basic.up_frame conn
            frame.Frame.event.ev.Instruct.ev_stacksize
        with
      | None -> Lwt.return None
      | Some (stack_pos, pc) -> (
          Log.debug (fun m -> m "up_frame %s" (Pc.show pc));%lwt
          try%lwt
            let frame' =
              {
                Frame.index = frame.index + 1;
                stack_pos;
                event = Symbols.find_event conn#symbols pc;
              }
            in
            Lwt.return (Some frame')
          with Not_found -> Lwt.return None ) )
        [%finally Debugcom_basic.set_frame conn stack_pos0])

let set_frame (conn : conn) frame =
  conn#lock (fun conn -> Debugcom_basic.set_frame conn frame.Frame.stack_pos)

let exec_with_frame (conn : conn) index f =
  conn#lock (fun conn ->
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
      (f frame) [%finally set_frame conn frame0])
