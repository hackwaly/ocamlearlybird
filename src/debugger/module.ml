open Symbols
open Util

type t = Symbols.module_ = {
  frag : int;
  id : string;
  source : string option;
  events : event array;
}

let source_content t =
  let%lwt content, _ =
    Lwt_util.file_content_and_bols (t.source |> Option.get)
  in
  Lwt.return content

let source_line_start t line =
  [%lwt assert (line >= 1)];%lwt
  let%lwt _, bols =
    Lwt_util.file_content_and_bols (t.source |> Option.get)
  in
  Lwt.return bols.(line - 1)

let source_line_length t line =
  [%lwt assert (line >= 1)];%lwt
  let%lwt _, bols =
    Lwt_util.file_content_and_bols (t.source |> Option.get)
  in
  Lwt.return (bols.(line) - bols.(line - 1))

let line_column_to_cnum t line column =
  let%lwt _, bols =
    Lwt_util.file_content_and_bols (t.source |> Option.get)
  in
  let bol = bols.(line - 1) in
  let cnum = bol + column in
  Lwt.return cnum

let to_seq_events m = m.events |> Array.to_seq

let find_event t line column =
  let expand_to_equivalent_range code cnum =
    (* TODO: Support skip comments *)
    let is_whitespace_or_semicolon c =
      match c with ' ' | '\t' | '\r' | '\n' | ';' -> true | _ -> false
    in
    assert (cnum >= 0 && cnum < String.length code);
    let c = code.[cnum] in
    if is_whitespace_or_semicolon c then
      let rec aux f n =
        let n' = f n in
        let c = code.[n'] in
        if is_whitespace_or_semicolon c then aux f n' else Lwt.return n
      in
      let%lwt l = aux (fun x -> x - 1) cnum in
      let%lwt r = aux (fun x -> x + 1) cnum in
      Lwt.return (l, r + 1)
    else Lwt.return (cnum, cnum)
  in
  let find code events cnum =
    let%lwt l, r = expand_to_equivalent_range code cnum in
    assert (l <= r);
    let cmp ev () =
      let ev_cnum = (Debug_event.lexing_position ev.Symbols.ev).pos_cnum in
      if ev_cnum < l then -1 else if ev_cnum > r then 1 else 0
    in
    Lwt.return
      ( match events |> Array_util.bsearch ~cmp () with
      | `At i -> events.(i)
      | _ -> raise Not_found )
  in
  let%lwt code, bols =
    Lwt_util.file_content_and_bols (t.source |> Option.get)
  in
  let bol = bols.(line - 1) in
  let cnum = bol + column in
  let%lwt ev = find code t.events cnum in
  Lwt.return ev
