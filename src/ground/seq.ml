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

open Stdlib
include Seq

let tap f t =
  Seq.map
    (fun it ->
      f it;
      it)
    t

let rec find_map_opt f seq =
  match seq () with
  | Seq.Nil -> None
  | Seq.Cons (hd, tl) -> (
      match f hd with Some r -> Some r | None -> find_map_opt f tl )

let int_range ?(start = 0) ?end_ () =
  let i = ref start in
  let rec next () =
    let v = !i in
    match end_ with
    | Some end_ when v > end_ -> Nil
    | _ ->
        incr i;
        Seq.Cons (v, next)
  in
  next
