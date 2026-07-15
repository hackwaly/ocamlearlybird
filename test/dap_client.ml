(* A minimal DAP client, used by the integration tests to drive ocamlearlybird
   the way an editor would: spawn the adapter, launch a bytecode program, stop
   at a breakpoint and inspect the frames.

   It talks to the adapter over the protocol rather than linking against its
   internals.

   The tests print a transcript on stdout which dune diffs against the recorded
   .expected file, so anything printed by a test must be stable across the OCaml
   versions we support (4.12 .. 5.x) and across platforms. *)

open Debug_protocol

(* ocamlearlybird extends the standard launch arguments; [program] is the
   bytecode executable to debug. See src/adapter/debug_protocol_ex.ml. *)
module Launch_command = struct
  let type_ = Launch_command.type_

  module Arguments = struct
    type t = {
      name : string;
      program : string;
      stop_on_entry : bool; [@key "stopOnEntry"]
      console : string;
    }
    [@@deriving yojson { strict = false }]
  end

  module Result = Launch_command.Result
end

type t = {
  rpc : Debug_rpc.t;
  proc : Lwt_process.process;
  stopped : Stopped_event.Payload.t Lwt_stream.t;
}

(* Fail rather than hang a CI run forever. *)
let timeout = 60.0

let with_timeout what f =
  Lwt.pick
    [
      f ();
      (Lwt_unix.sleep timeout;%lwt
       Lwt.fail_with
         (Printf.sprintf "timed out after %.0fs waiting for %s" timeout what));
    ]

(* [launch] must not wait for the launch response before setting breakpoints:
   the adapter announces it is ready with an initialized event, expects the
   breakpoints and a configurationDone in between, and only then replies. *)
let launch ~adapter ~program ~source ~breakpoints =
  let proc = Lwt_process.open_process (adapter, [| adapter; "debug" |]) in
  let rpc = Debug_rpc.create ~in_:proc#stdout ~out:proc#stdin () in
  let stopped =
    Debug_rpc.event rpc (module Stopped_event) |> Lwt_react.E.to_stream
  in
  let initialized =
    Debug_rpc.event rpc (module Initialized_event) |> Lwt_react.E.to_stream
  in
  Lwt.async (fun () -> Debug_rpc.start rpc);
  let%lwt _ =
    Debug_rpc.exec_command rpc
      (module Initialize_command)
      Initialize_command.Arguments.(
        make ~adapter_id:"ocaml" ~lines_start_at1:(Some true)
          ~columns_start_at1:(Some true) ())
  in
  let launched =
    Debug_rpc.exec_command rpc
      (module Launch_command)
      Launch_command.Arguments.
        {
          name = "integration test";
          program;
          stop_on_entry = false;
          console = "internalConsole";
        }
  in
  with_timeout "initialized event" (fun () -> Lwt_stream.next initialized);%lwt
  let%lwt _ =
    Debug_rpc.exec_command rpc
      (module Set_breakpoints_command)
      Set_breakpoints_command.Arguments.(
        make
          ~source:Source.(make ~path:(Some source) ())
          ~breakpoints:
            (Some
               (breakpoints |> List.map (fun line -> Source_breakpoint.(make ~line ()))))
          ())
  in
  let%lwt _ =
    Debug_rpc.exec_command rpc
      (module Configuration_done_command)
      ()
  in
  let%lwt () = with_timeout "launch response" (fun () -> launched) in
  Lwt.return { rpc; proc; stopped }

let wait_stopped t =
  with_timeout "stopped event" (fun () -> Lwt_stream.next t.stopped)

let stack_trace t ~thread_id =
  let%lwt res =
    Debug_rpc.exec_command t.rpc
      (module Stack_trace_command)
      Stack_trace_command.Arguments.(make ~thread_id ())
  in
  Lwt.return res.Stack_trace_command.Result.stack_frames

let top_frame t ~thread_id =
  match%lwt stack_trace t ~thread_id with
  | [] -> Lwt.fail_with "no stack frames"
  | frame :: _ -> Lwt.return frame

(* The scopes of [frame], each with its variables, in the order the adapter
   reports them.

   The pseudo-variables the adapter synthesises (%accu, the bytecode
   accumulator) are dropped. Whether they show up depends on the kind of debug
   event the breakpoint landed on. *)
let scopes t ~(frame : Stack_frame.t) =
  let%lwt res =
    Debug_rpc.exec_command t.rpc
      (module Scopes_command)
      Scopes_command.Arguments.(make ~frame_id:frame.id)
  in
  res.Scopes_command.Result.scopes
  |> Lwt_list.map_s (fun (scope : Scope.t) ->
         let%lwt res =
           Debug_rpc.exec_command t.rpc
             (module Variables_command)
             Variables_command.Arguments.(
               make ~variables_reference:scope.variables_reference ())
         in
         let variables =
           res.Variables_command.Result.variables
           |> List.filter (fun (variable : Variable.t) ->
                  not (String.length variable.name > 0 && variable.name.[0] = '%'))
         in
         Lwt.return (scope.name, variables))

let scope t ~frame ~name =
  let%lwt scopes = scopes t ~frame in
  match List.assoc_opt name scopes with
  | Some variables -> Lwt.return variables
  | None -> Lwt.fail_with (Printf.sprintf "no %s scope" name)

(* One level of children of a structured value (the elements of a list, the
   fields of a record, ...).

   Values expose named children (record fields, closure captures) and indexed
   children (array and list elements) separately, and the adapter expects them
   to be fetched separately, with a filter, the way VS Code does when a value
   reports both counts. *)
let expand t ~(variable : Variable.t) =
  let open Variables_command.Arguments in
  let reference = variable.variables_reference in
  let vars args =
    let%lwt res = Debug_rpc.exec_command t.rpc (module Variables_command) args in
    Lwt.return res.Variables_command.Result.variables
  in
  let named = Option.value variable.named_variables ~default:0 in
  let indexed = Option.value variable.indexed_variables ~default:0 in
  if reference = 0 then Lwt.return []
  else if named = 0 && indexed = 0 then
    (* Counts unknown; a single unfiltered request returns everything. *)
    vars (make ~variables_reference:reference ())
  else
    let%lwt named =
      if named > 0 then
        vars (make ~variables_reference:reference ~filter:(Some Filter.Named) ())
      else Lwt.return []
    in
    let%lwt indexed =
      if indexed > 0 then
        vars
          (make ~variables_reference:reference ~filter:(Some Filter.Indexed)
             ~start:(Some 0) ~count:(Some indexed) ())
      else Lwt.return []
    in
    Lwt.return (named @ indexed)

(* Stepping. Each returns the frame the debuggee comes to rest in. *)

let step t ~thread_id command =
  let%lwt () = command () in
  let%lwt _ = wait_stopped t in
  top_frame t ~thread_id

let next t ~thread_id =
  step t ~thread_id (fun () ->
      Debug_rpc.exec_command t.rpc
        (module Next_command)
        Next_command.Arguments.(make ~thread_id ()))

let step_in t ~thread_id =
  step t ~thread_id (fun () ->
      Debug_rpc.exec_command t.rpc
        (module Step_in_command)
        Step_in_command.Arguments.(make ~thread_id ()))

let step_out t ~thread_id =
  step t ~thread_id (fun () ->
      Debug_rpc.exec_command t.rpc
        (module Step_out_command)
        Step_out_command.Arguments.(make ~thread_id ()))

let continue t ~thread_id =
  step t ~thread_id (fun () ->
      let%lwt _ =
        Debug_rpc.exec_command t.rpc
          (module Continue_command)
          Continue_command.Arguments.(make ~thread_id ())
      in
      Lwt.return ())

let string_of_reason (reason : Stopped_event.Payload.Reason.t) =
  match reason with
  | Breakpoint -> "breakpoint"
  | Entry -> "entry"
  | Step -> "step"
  | Exception -> "exception"
  | Pause -> "pause"
  | _ -> "other"

let disconnect t =
  (try%lwt
     Debug_rpc.exec_command t.rpc
       (module Disconnect_command)
       Disconnect_command.Arguments.(make ~terminate_debuggee:(Some true) ())
   with _ -> Lwt.return ());%lwt
  t.proc#terminate;
  Lwt.return ()

(* Run [f] against a freshly launched adapter, tearing it down afterwards. *)
let with_session ~program ~source ~breakpoints f =
  let adapter =
    match Sys.getenv_opt "EARLYBIRD_ADAPTER" with
    | Some adapter -> adapter
    | None -> failwith "EARLYBIRD_ADAPTER is not set"
  in
  (* The adapter resolves the debuggee's sources relative to its own working
     directory, using the search dirs recorded in the bytecode; so it is run
     from the directory the fixture was compiled in, and everything handed to it
     has to be an absolute path. *)
  let absolute path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  in
  let adapter = absolute adapter in
  let program = absolute program in
  let source = absolute source in
  Sys.chdir (Filename.dirname program);
  let%lwt t = launch ~adapter ~program ~source ~breakpoints in
  (f t) [%finally disconnect t]
