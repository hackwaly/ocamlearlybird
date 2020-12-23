open Remote_debugger

type scope = Local | Heap | Rec | Global

type value =
  | Int of int
  | Double of float
  | Bool of bool
  | Char of char
  | String of string
  | Module of Symbols.Module.t
  | Scope of scope
  | Unknown

type obj = { scene_id : int64; value : value; name : string }

type ident = Ident of string | Qualified of string * ident

type expr = Lookup of ident | Field of expr * ident | Component of expr * int

type scene = {
  id : int64;
  report : report;
  event : Instruct.debug_event;
  typenv : Env.t;
}

type t = {
  symbols : Symbols.t;
  mutable symbol_version : int;
  remote_debugger : (module Remote_debugger.S);
  conn : conn;
  mutable scene : scene option;
}

(*TODO: Use lwt.detach*)

let create ~symbols ~remote_debugger ~conn () =
  let source_dirs = Symbols.source_dirs symbols in
  Load_path.init source_dirs;
  Envaux.reset_cache ();
  {
    symbols;
    remote_debugger;
    conn;
    symbol_version = Symbols.version symbols;
    scene = None;
  }

let update_scene t report =
  if Symbols.version t.symbols <> t.symbol_version then (
    let source_dirs = Symbols.source_dirs t.symbols in
    Load_path.init source_dirs;
    Envaux.reset_cache ();
    t.symbol_version <- Symbols.version t.symbols );
  let event = Symbols.find_event t.symbols report.rep_program_pointer in
  let typenv = Envaux.env_from_summary event.ev_typenv event.ev_typsubst in
  let scene = { id = report.rep_event_count; report; event; typenv } in
  t.scene <- Some scene

let scope_name scope =
  match scope with
  | Local -> "local"
  | Heap -> "heap"
  | Rec -> "rec"
  | Global -> "global"

let find_ty env path =
  let val_desc = env |> Env.find_value path in
  let ty = Ctype.correct_levels val_desc.Types.val_type in
  ty

let make_value t =
  let (module Rdbg) = t.remote_debugger in
  let conn = t.conn in
  let scene = t.scene |> Option.get in
  fun rv ty ->
    let marshal f =
      let%lwt mv = Rdbg.marshal_obj conn rv in
      Lwt.return (f mv)
    in
    if Ctype.matches scene.typenv Predef.type_int ty then
      marshal (fun v -> Int v)
    else Lwt.return Unknown

let list_local t =
  let (module Rdbg) = t.remote_debugger in
  let conn = t.conn in
  let scene = t.scene |> Option.get in
  let ce_stack = scene.event.ev_compenv.ce_stack in
  let make_obj (id, index) =
    let ty = find_ty scene.typenv (Path.Pident id) in
    let%lwt rv = Rdbg.get_local conn index in
    let%lwt value = make_value t rv ty in
    let obj =
      {
        scene_id = scene.id;
        value;
        name = Ident.name id;
      }
    in
    Lwt.return obj
  in
  let%lwt objs =
    let iter f = Ident.iter (fun id index -> f (id, index)) ce_stack in
    iter |> Iter.to_list |> Lwt_list.map_s make_obj
  in
  Lwt.return objs

let is_valid t obj =
  match t.scene with
  | Some scene -> scene.id = obj.scene_id
  | None -> false

let scope_obj t scope =
  let scene = t.scene |> Option.get in
  ignore t;
  let obj =
    {
      scene_id = scene.id;
      value = Scope scope;
      name = scope_name scope;
    }
  in
  Lwt.return obj

let list_obj t obj =
  [%lwt assert (is_valid t obj)];%lwt
  match obj.value with Scope Local -> list_local t | _ -> [%lwt assert false]
