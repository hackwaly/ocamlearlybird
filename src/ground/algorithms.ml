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

let binary_search ~cmp ~get container left right key =
  let rec aux left right =
    if left > right then `Just_after right
    else
      let middle = (left + right) / 2 in
      match cmp (get container middle) key with
      | 0 -> `At middle
      | n when n > 0 -> aux left (middle - 1)
      | _ -> aux (middle + 1) right
  in
  if left >= right then `Empty else aux left right
