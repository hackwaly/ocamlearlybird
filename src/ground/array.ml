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
include Array

module Sorted = struct
  let bsearch ~cmp k a =
    Algorithms.binary_search ~cmp ~get:Array.get a 0 (Array.length a) k

  let slice_bs ~cmp start end_ a =
    let start =
      match a |> bsearch ~cmp start with
      | `Empty -> 0
      | `At i -> i
      | `Just_after i -> i + 1
    in
    let end_ =
      match a |> bsearch ~cmp end_ with
      | `Empty -> 0
      | `At i -> i
      | `Just_after i -> min (i + 1) (Array.length a)
    in
    Array.sub a start (end_ - start)
end
