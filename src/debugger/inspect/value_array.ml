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

open Value_basic

class array_value ~scene ~typenv ~obj ~element_type ~len () =
  object
    inherit value

    method to_short_string = if len = 0 then "[||]" else "[|…|]"

    method! num_indexed = len

    method! num_named = 1

    method! list_named =
      Lwt.return [ ("‹length›", new Value_simple.int_value len) ]

    method! get_indexed i =
      let%lwt obj' = Scene.get_field scene obj i in
      adopt scene typenv obj' element_type
  end

let adopter scene typenv obj typ =
  match Types.get_desc typ with
  | Tconstr (_, [ element_type ], _)
    when Typenv.type_matches typenv (Predef.type_array element_type) typ
         && Scene.is_block obj ->
      let%lwt len = Scene.get_size scene obj in
      Lwt.return
        (Some (new array_value ~scene ~typenv ~obj ~element_type ~len ()))
  | _ -> Lwt.return None
