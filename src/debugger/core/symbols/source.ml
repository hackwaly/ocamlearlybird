type t = { path : string; content : string; bols : int array }

let from_path path =
  let%lwt content =
    let%lwt ic = Lwt_io.open_file ~mode:Lwt_io.input path in
    (Lwt_io.read ic) [%finally Lwt_io.close ic]
  in
  let bols = ref [ 0 ] in
  for i = 0 to String.length content - 1 do
    if content.[i] = '\n' then bols := (i + 1) :: !bols
  done;
  let bols = !bols |> List.rev |> Array.of_list in
  Lwt.return { path; content; bols }
