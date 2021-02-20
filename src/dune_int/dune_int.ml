open Ground
open Sexplib

module Csexp = Csexp.Make (Sexp)

type library_info = {
  source_dir : string;
  include_dirs : string list;
}

type module_info = {
  module_id : string;
  source : string option;
  library : library_info;
}

type t = {
  module_tbl : (string, module_info) Hashtbl.t;
}

let init ~workspace ~program () =
  Log.debug (fun m -> m "workspace: %s" workspace);
  let cmd = Filename.quote_command "dune" ["describe"; "--lang"; "0.1"; "--format"; "csexp"] in
  let%lwt desc = Lwt_process.(pread ~cwd:workspace (shell cmd)) in
  let desc = Csexp.parse_string desc |> Result.get_ok in
  Log.debug (fun m -> m "%s" (Sexp.to_string desc));
  Lwt.return ()
