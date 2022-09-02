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

class forced_value v =
  object
    inherit value

    method to_short_string = "«lazy.is_val»"

    method! num_named = 1

    method! list_named = Lwt.return [ ("‹val›", v) ]
  end

class lazy_value f =
  object
    inherit value

    method to_short_string = "«lazy»"

    method! num_named = 1

    method! list_named = Lwt.return [ ("‹fun›", f) ]
  end

let adopter scene typenv obj typ =
  match Types.get_desc typ with
  | Tconstr (_, [ rty ], _)
    when Typenv.type_matches typenv (Predef.type_lazy_t rty) typ ->
      let%lwt tag = Scene.get_tag scene obj in
      if tag <> Obj.lazy_tag then
        let%lwt vobj = Scene.get_field scene obj 0 in
        let%lwt v = adopt scene typenv vobj rty in
        Lwt.return (Some (new forced_value v))
      else
        let fty =
          Ctype.newty (Types.Tarrow (Nolabel, Predef.type_unit, rty, Types.commu_ok))
        in
        let%lwt fobj = Scene.get_field scene obj 0 in
        let%lwt f = adopt scene typenv fobj fty in
        Lwt.return (Some (new lazy_value f))
  | _ -> Lwt.return None
