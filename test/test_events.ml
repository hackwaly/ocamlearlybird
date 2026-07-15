(* Map source lines to debug events, the way setting a breakpoint does.

   Code_module.find_event snaps a requested line to a nearby debug event as long
   as only whitespace and comments separate the two (Trivia_check), while
   find_events returns the events that fall within the line itself. Between them
   they decide which lines a breakpoint can be set on, and where it ends up.

   This is exercised directly on the symbol table, without running a debuggee,
   so a failure points at the mapping rather than at the adapter. *)

open Debugger

let module_of_bytecode ~program ~module_id =
  let%lwt debug_info = Bytecode.load_debuginfo program in
  let frag = Code_fragment.make 0 debug_info in
  (* Resolving the fragment reads the sources, which find_event needs to know
     where the lines of the module start. *)
  let symbols = Symbols.create () in
  Symbols.add_fragment symbols frag;%lwt
  Lwt.return (Code_fragment.find_module frag module_id)

let position (event : Instruct.debug_event) =
  let pos = event.ev_loc.loc_start in
  (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)

let main () =
  let%lwt module_ =
    module_of_bytecode ~program:"fixtures/hello.bc" ~module_id:"Hello"
  in
  let source = In_channel.with_open_text "fixtures/hello.ml" In_channel.input_all in
  (* Drop the empty string after the file's trailing newline so we only query
     lines that actually exist. *)
  let lines =
    match List.rev (String.split_on_char '\n' source) with
    | "" :: rest -> List.rev rest
    | _ -> String.split_on_char '\n' source
  in
  let describe f =
    match f () with
    | events ->
        events
        |> List.map (fun event ->
               let line, column = position event in
               Printf.sprintf "%d:%d" line column)
    | exception Not_found -> []
  in
  lines
  |> List.iteri (fun i line ->
         let line_no = i + 1 in
         (* Where a breakpoint on this line would end up ... *)
         let event =
           match describe (fun () -> [ Code_module.find_event module_ ~line:line_no () ]) with
           | [] -> "none"
           | positions -> String.concat "; " positions
         in
         (* ... and the events the line itself contains. *)
         let within =
           describe (fun () -> Code_module.find_events module_ ~line:line_no ())
         in
         Printf.printf "%2d  snaps to %-6s  contains [%s]  | %s\n" line_no event
           (String.concat "; " within) line);
  Lwt.return ()

let () = Lwt_main.run (main ())
