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

class object_value ~scene ~typenv ~obj ~members () =
  object
    inherit value

    method to_short_string = "«object»"

    method! num_named = List.length members

    method! list_named =
      let%lwt table = Scene.get_field scene obj 0 in
      let%lwt num_methods = Scene.get_field scene table 0 in
      let%lwt num_methods = Scene.marshal_obj scene num_methods in
      let find_method name =
        let left = ref 0 in
        let right = ref num_methods in
        let tag = CamlinternalOO.public_method_label name in
        while%lwt !left < !right do
          let middle = (!left + !right) / 2 in
          let%lwt tag' = Scene.get_field scene table (2 + (2 * middle) + 1) in
          let%lwt tag' = Scene.marshal_obj scene tag' in
          if tag <= tag' then right := middle else left := middle + 1;
          Lwt.return ()
        done;%lwt
        let%lwt method' = Scene.get_field scene table (2 + (2 * !left)) in
        Lwt.return method'
      in
      members
      |> Lwt_list.map_s (fun (name, _, ty) ->
             let ty =
               Ctype.newty (Types.Tarrow (Nolabel, Predef.type_unit, ty, Types.commu_ok))
             in
             let%lwt meth = find_method name in
             let%lwt value = adopt scene typenv meth ty in
             Lwt.return (name, value))
  end

let adopter scene typenv obj ty =
  match Types.get_desc ty with
  | Tobject (fields_ty, _) ->
      let members, _ = Ctype.flatten_fields fields_ty in
      Lwt.return (Some (new object_value ~scene ~typenv ~obj ~members ()))
  | _ -> Lwt.return None
