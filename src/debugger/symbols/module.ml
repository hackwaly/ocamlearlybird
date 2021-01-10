open Debuginfo
open Util

type t = Debuginfo.module_ = {
  frag : int;
  id : string;
  source : string option;
  mutable events : event array;
}

let source_content t =
  let%lwt content, _ =
    Lwt_util.file_content_and_bols (t.source |> Option.get)
  in
  Lwt.return content

let source_line_start t line =
  [%lwt assert (line >= 1)];%lwt
  let%lwt _, bols = Lwt_util.file_content_and_bols (t.source |> Option.get) in
  Lwt.return bols.(line - 1)

let source_line_length t line =
  [%lwt assert (line >= 1)];%lwt
  let%lwt _, bols = Lwt_util.file_content_and_bols (t.source |> Option.get) in
  Lwt.return (bols.(line) - bols.(line - 1))

let line_column_to_cnum t line column =
  let%lwt _, bols = Lwt_util.file_content_and_bols (t.source |> Option.get) in
  let bol = bols.(line - 1) in
  let cnum = bol + column - 1 in
  Lwt.return cnum

let to_seq_events m = m.events |> Array.to_seq

let find_event_by_cnum t cnum =
  let find code events cnum =
    let cmp ev () =
      let ev_cnum = (Debug_event.lexing_position ev.Debuginfo.ev).pos_cnum in
      if ev_cnum < cnum then -1 else if ev_cnum > cnum then 1 else 0
    in
    let check i =
      if i < 0 || i >= Array.length events then None
      else
        let ev = events.(i) in
        let cnum' = (Debug_event.lexing_position ev.Debuginfo.ev).pos_cnum in
        if cnum' = -1 then None
        else if cnum' = cnum then Some ev
        else
          let str =
            if cnum' < cnum then String.sub code cnum' (cnum - cnum')
            else String.sub code cnum (cnum' - cnum)
          in
          if Trivia_check.check str then Some ev else None
    in
    match events |> Array_util.bsearch ~cmp () with
    | `At i -> check i
    | `All_lower -> check (Array.length events - 1)
    | `Just_after i -> [ i; i + 1 ] |> List.find_map check
    | _ -> None
  in
  let%lwt code, _ = Lwt_util.file_content_and_bols (t.source |> Option.get) in
  match find code t.events cnum with
  | Some ev -> Lwt.return ev
  | None -> raise Not_found

let find_event t line column =
  let%lwt _, bols = Lwt_util.file_content_and_bols (t.source |> Option.get) in
  let bol = bols.(line - 1) in
  let cnum = bol + column - 1 in
  let%lwt ev = find_event_by_cnum t cnum in
  Lwt.return ev
