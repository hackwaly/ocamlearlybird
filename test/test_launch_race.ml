(* Regression test for the initialize/launch handler race. A client that
   pipelines "launch" right after "initialize", sending both before reading the
   initialize response, must still have its launch handled and receive the
   "initialized" event. Before the fix the launch could arrive before the
   adapter had registered the Launch handler and was silently dropped, so no
   "initialized" event ever came (see the intermittent CI timeout).

   This talks to the adapter with raw framing rather than the Debug_rpc client,
   so we can pipeline the two requests. *)

let adapter =
  match Sys.getenv_opt "EARLYBIRD_ADAPTER" with
  | Some adapter -> adapter
  | None -> failwith "EARLYBIRD_ADAPTER is not set"

let program = Filename.concat (Sys.getcwd ()) "fixtures/hello.bc"

let frame seq command arguments =
  let body =
    Printf.sprintf {|{"seq":%d,"type":"request","command":"%s","arguments":%s}|}
      seq command arguments
  in
  Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length body) body

(* Whether [sub] occurs in [s]. *)
let contains_substring s sub =
  let n = String.length s and m = String.length sub in
  let rec at i = i + m <= n && (String.sub s i m = sub || at (i + 1)) in
  m = 0 || at 0

(* Read one DAP message and return its raw JSON body. *)
let read_message ic =
  let rec headers content_length =
    let%lwt line = Lwt_io.read_line ic in
    match String.trim line with
    | "" -> Lwt.return content_length
    | line -> (
        match String.split_on_char ':' line with
        | [ key; value ] when String.trim key = "Content-Length" ->
            headers (int_of_string (String.trim value))
        | _ -> headers content_length)
  in
  let%lwt length = headers 0 in
  let buf = Bytes.create length in
  Lwt_io.read_into_exactly ic buf 0 length;%lwt
  Lwt.return (Bytes.to_string buf)

let main () =
  let proc = Lwt_process.open_process (adapter, [| adapter; "debug" |]) in
  let init = frame 1 "initialize" {|{"adapterID":"ocaml"}|} in
  let launch =
    frame 2 "launch"
      (Printf.sprintf
         {|{"name":"race","program":%S,"stopOnEntry":false,"console":"internalConsole"}|}
         program)
  in
  Lwt_io.write proc#stdin (init ^ launch);%lwt
  Lwt_io.flush proc#stdin;%lwt
  let rec wait_initialized () =
    let%lwt message = read_message proc#stdout in
    if contains_substring message {|"initialized"|} then
      Lwt.return "received the initialized event"
    else wait_initialized ()
  in
  let timeout =
    Lwt_unix.sleep 30.0;%lwt
    Lwt.return "timed out waiting for the initialized event"
  in
  let%lwt result = Lwt.pick [ wait_initialized (); timeout ] in
  print_endline result;
  proc#terminate;
  Lwt.return ()

let () = Lwt_main.run (main ())
