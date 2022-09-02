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

class func_value ?pc ?loc () =
  let () = ignore pc in
  object
    inherit value

    method to_short_string = "«fun»"

    method! vscode_menu_context = Some "ocamlearlybird.function"

    method! closure_code_location = loc

    method! num_named = 1

    method! list_named =
      Lwt.return
        [
          ( "‹tips›",
            new tips_value
              [| "You can use context menu to goto closure code location." |] );
        ]
  end

let adopter scene typenv obj typ =
  ignore typenv;
  match Types.get_desc typ with
  | Types.Tarrow _ ->
      let%lwt pc, loc =
        if Scene.is_block obj then
          let%lwt pc, loc = Scene.get_closure_code scene obj in
          Lwt.return (Some pc, loc)
        else Lwt.return (None, None)
      in
      Lwt.return (Some (new func_value ?pc ?loc ()))
  | _ -> Lwt.return None
