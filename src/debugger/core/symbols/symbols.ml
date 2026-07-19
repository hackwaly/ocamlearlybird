open Ground

type t = {
  get_source_dir : string -> string option;
  debug_filter : string -> bool;
  workspace_dirs : string list;
  mutable frags : Code_fragment.t Map.Make(Int).t;
  mutable source_module_by_digest : Code_module.t Map.Make(Digest).t;
  mutable version : int;
  dummy : unit;
}

module IntMap_ = Map.Make (Int)
module DigestMap_ = Map.Make (Digest)

let create ?(get_source_dir = fun _ -> None) ?(debug_filter = fun _ -> true)
    ?(workspace_dirs = []) () =
  {
    get_source_dir;
    debug_filter;
    workspace_dirs;
    frags = IntMap_.empty;
    source_module_by_digest = DigestMap_.empty;
    version = 0;
    dummy = ();
  }

(* Since dune 3.0, [map_workspace_root] is on by default and rewrites the build
   directory prefix in the debug info to a fixed "/workspace_root", which does
   not exist on disk, so a module's source cannot be found there. Rewrite that
   prefix back to the real directories derived from the executable's location
   ([workspace_dirs]). A dir with no such prefix is left as is. *)
let workspace_root_prefix = "/workspace_root"

let remap_dir workspace_dirs dir =
  let n = String.length workspace_root_prefix in
  let has_prefix =
    String.length dir >= n
    && String.sub dir 0 n = workspace_root_prefix
    && (String.length dir = n || dir.[n] = '/')
  in
  if has_prefix && workspace_dirs <> [] then
    let suffix = String.sub dir n (String.length dir - n) in
    List.map (fun root -> root ^ suffix) workspace_dirs
  else [ dir ]

(* The real directories that dune's "/workspace_root" stands for, derived from
   the executable's path. A dune executable lives at
   <root>/_build/<context>/<...>, and dune mirrors the source tree under the
   build context, so both the source root <root> and the build context
   <root>/_build/<context> hold the sources (byte-identical copies). The source
   root is listed first so a resolved source is the user's own file rather than
   the build copy. Returns [] when the path is not under a "_build" directory,
   in which case no rewriting happens. *)
let derive_workspace_dirs executable =
  let executable =
    if Filename.is_relative executable then
      Filename.concat (Sys.getcwd ()) executable
    else executable
  in
  let marker = "/_build/" in
  let marker_len = String.length marker in
  let len = String.length executable in
  let rec find i =
    if i + marker_len > len then None
    else if String.sub executable i marker_len = marker then Some i
    else find (i + 1)
  in
  match find 0 with
  | None -> []
  | Some i ->
      let source_root = String.sub executable 0 i in
      let after =
        String.sub executable (i + marker_len) (len - i - marker_len)
      in
      let context =
        match String.index_opt after '/' with
        | Some j -> String.sub after 0 j
        | None -> after
      in
      [ source_root; source_root ^ marker ^ context ]

let dup t = { t with dummy = () }

let add_fragment t frag =
  let resolve_module_source module_id search_dirs =
    let search_dirs =
      match t.get_source_dir module_id with
      | Some dir -> [ dir ]
      | None -> search_dirs
    in
    let search_dirs =
      search_dirs |> List.concat_map (remap_dir t.workspace_dirs)
    in
    let module_id' =
      Str.split (Str.regexp "__") module_id |> List.rev |> List.hd
    in
    let source_paths =
      search_dirs |> List.to_seq
      |> Seq.flat_map (fun dir ->
             List.to_seq
               [
                 dir ^ "/" ^ String.uncapitalize_ascii module_id' ^ ".ml";
                 dir ^ "/" ^ String.uncapitalize_ascii module_id' ^ ".re";
                 dir ^ "/" ^ module_id' ^ ".ml";
                 dir ^ "/" ^ module_id' ^ ".re";
               ])
      |> List.of_seq
    in
    source_paths |> Lwt_list.find_s Lwt_unix.file_exists
  in
  let resolve_module (module_ : Code_module.t) =
    match%lwt resolve_module_source module_.module_id module_.search_dirs with
    | source_path ->
        let%lwt source = Source.from_path source_path in
        t.source_module_by_digest <-
          t.source_module_by_digest |> DigestMap_.add source.digest module_;
        module_.source <- Some source;
        Lwt.return ()
    | exception Not_found -> Lwt.return ()
  in
  let resolve_fragment frag =
    frag |> Code_fragment.to_modules_seq |> Lwt_seq.iter_s resolve_module
  in
  resolve_fragment frag;%lwt
  t.frags <- t.frags |> IntMap_.add frag.num frag;
  t.version <- t.version + 1;
  Lwt.return ()

let remove_fragment t frag_num =
  t.source_module_by_digest <-
    t.source_module_by_digest
    |> DigestMap_.filter (fun _ (module_ : Code_module.t) ->
           module_.frag <> frag_num);
  t.frags <- t.frags |> IntMap_.remove frag_num;
  t.version <- t.version + 1

let find_fragment t frag_num = t.frags |> IntMap_.find frag_num

let find_event t (frag_num, code_pos) =
  let frag = find_fragment t frag_num in
  Hashtbl.find frag.event_tbl code_pos

let find_event_opt t pc = try Some (find_event t pc) with Not_found -> None

let to_fragments_seq t = t.frags |> IntMap_.to_seq |> Seq.map snd

let find_source_module source t =
  let digest = Digest.file source in
  t.source_module_by_digest |> DigestMap_.find digest

let to_source_modules_seq t =
  t.source_module_by_digest |> DigestMap_.to_seq |> Seq.map snd
