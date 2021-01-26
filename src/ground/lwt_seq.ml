type 'a t = 'a Seq.t

let rec iter_s f seq =
  match seq () with
  | Seq.Nil -> Lwt.return ()
  | Seq.Cons (hd, tl) ->
      let%lwt () = f hd in
      iter_s f tl

let find_map_s f seq =
  let rec aux seq =
    match seq () with
    | Seq.Nil -> raise Not_found
    | Seq.Cons (it, seq) -> (
        match%lwt f it with Some r -> Lwt.return r | None -> aux seq )
  in
  aux seq

let rec find_opt_s f seq =
  match seq () with
  | Seq.Nil -> Lwt.return None
  | Seq.Cons (hd, tl) ->
      if%lwt f hd then Lwt.return (Some hd) else find_opt_s f tl
