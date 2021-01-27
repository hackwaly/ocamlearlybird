open Ground

exception No_source

type t = {
  frag : int;
  module_id : string;
  mutable source : Source.t option;
  (* Only populated after added to symbols *)
  search_dirs : string list;
  events : Instruct.debug_event array;
}

let _get_content t =
  match t.source with
  | Some { content; _ } -> content
  | None -> raise No_source

let _get_bols t =
  match t.source with
  | Some { bols; _ } -> bols
  | None -> raise No_source

let _find_event_index_by_cnum t cnum =
  let find content events cnum =
    let cmp ev () =
      let ev_cnum = Util.Debug_event.cnum ev in
      if ev_cnum < cnum then -1 else if ev_cnum > cnum then 1 else 0
    in
    let test_index i =
      if i < 0 || i >= Array.length events then false
      else
        let ev = events.(i) in
        let cnum' = Util.Debug_event.cnum ev in
        if cnum' = -1 then false
        else if cnum' = cnum then true
        else
          let str =
            if cnum' < cnum then String.sub content cnum' (cnum - cnum')
            else String.sub content cnum (cnum' - cnum)
          in
          if Trivia_check.check str then true else false
    in
    let check i = if test_index i then Some i else None in
    match events |> Array.Sorted.bsearch ~cmp () with
    | `Empty -> None
    | `At i -> check i
    | `Just_after i -> [ i; i + 1 ] |> List.find_map check
  in
  let content = _get_content t in
  match find content t.events cnum with
  | Some i -> i
  | None -> raise Not_found

let find_event t ~line ?(column = 1) () =
  try
    let bols = _get_bols t in
    let bol = bols.(line - 1) in
    let cnum = bol + column - 1 in
    let i = _find_event_index_by_cnum t cnum in
    t.events.(i)
  with No_source -> raise Not_found

let find_events t ~line ?(column = 1) ?end_line ?end_column () =
  try
    let bols = _get_bols t in
    let end_line = end_line |> Option.value ~default:line in
    let end_column =
      match end_column with
      | Some end_column -> end_column
      | None -> bols.(end_line) - bols.(end_line - 1)
    in
    let cmp = Compare.by2 Util.Debug_event.line_column (fun it -> it) in
    let evl =
      t.events
      |> Array.Sorted.slice_bs ~cmp (line, column) (end_line, end_column)
      |> Array.to_seq |> List.of_seq
    in
    evl
  with No_source -> raise Not_found
