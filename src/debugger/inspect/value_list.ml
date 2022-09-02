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

let nil_value =
  object
    inherit value

    method to_short_string = "[]"
  end

class list_value ~scene ~typenv ~obj ~element_type () =
  object
    inherit value

    method to_short_string = "‹hd› :: ‹tl›"

    method! num_named = 2

    method! list_named =
      let%lwt hd =
        let%lwt obj' = Scene.get_field scene obj 0 in
        adopt scene typenv obj' element_type
      in
      let%lwt tl =
        let%lwt obj' = Scene.get_field scene obj 1 in
        Lwt.return (new list_value ~scene ~typenv ~obj:obj' ~element_type ())
      in
      Lwt.return [ ("‹hd›", hd); ("‹tl›", tl) ]
  end

let adopter scene typenv obj typ =
  match Types.get_desc typ with
  | Tconstr (_, [ element_type ], _)
    when Typenv.type_matches typenv (Predef.type_list element_type) typ ->
      if Scene.is_block obj then
        Lwt.return (Some (new list_value ~scene ~typenv ~obj ~element_type ()))
      else Lwt.return (Some nil_value)
  | _ -> Lwt.return None
