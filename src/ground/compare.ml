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

type 'a t = 'a -> 'a -> int

let by ?(cmp = compare) f a b = cmp (f a) (f b)

let by2 ?(cmp = compare) f1 f2 a b = cmp (f1 a) (f2 b)

let opp cmp a b = cmp b a

let chain l a b =
  let rec aux l =
    match l with
    | [] -> 0
    | cmp :: l -> ( match cmp a b with 0 -> aux l | r -> r )
  in
  aux l

let to_equal cmp a b = cmp a b = 0
