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
