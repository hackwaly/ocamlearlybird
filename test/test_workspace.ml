(* Unit tests for the "/workspace_root" path handling that backs the dune source
   resolution fix (see Symbols.derive_workspace_dirs / Symbols.remap_dir).

   derive_workspace_dirs turns an executable path into the real directories that
   dune's "/workspace_root" stands for; remap_dir rewrites a recorded search dir
   using them. Both are pure, so they are checked directly here. *)

open Debugger

let show_list l = "[" ^ String.concat "; " l ^ "]"

let () =
  print_endline "== derive_workspace_dirs";
  [
    "/home/me/proj/_build/default/bin/main.bc";
    "/home/me/proj/_build/default/lib/sub/sub.bc";
    "/home/me/proj/_build/foo/bin/main.bc";
    (* not under a _build directory: nothing to derive *)
    "/home/me/proj/bin/main.bc";
  ]
  |> List.iter (fun path ->
         Printf.printf "  %s\n    -> %s\n" path
           (show_list (Symbols.derive_workspace_dirs path)));

  print_endline "== remap_dir";
  let workspace_dirs =
    [ "/home/me/proj"; "/home/me/proj/_build/default" ]
  in
  [
    (* rewritten against each workspace dir *)
    "/workspace_root";
    "/workspace_root/bin";
    "/workspace_root/lib/sub/.sub.objs/byte";
    (* a real path is left untouched ... *)
    "/home/me/.opam/lib/ocaml";
    (* ... and "/workspace_root" is only a prefix on a path boundary *)
    "/workspace_root_other/bin";
  ]
  |> List.iter (fun dir ->
         Printf.printf "  %s\n    -> %s\n" dir
           (show_list (Symbols.remap_dir workspace_dirs dir)));

  (* With no workspace dirs (executable not under _build), nothing is rewritten. *)
  Printf.printf "  (no workspace dirs) /workspace_root/bin\n    -> %s\n"
    (show_list (Symbols.remap_dir [] "/workspace_root/bin"))
