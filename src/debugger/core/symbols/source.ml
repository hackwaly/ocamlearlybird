(**
 * Copyright (C) 2021 Yuxiang Wen
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

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
