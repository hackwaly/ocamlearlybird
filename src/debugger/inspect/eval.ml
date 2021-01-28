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

open Ground

let value_path scene frame path =
  let rec address scene frame path =
    let event = frame.Frame.event |> Option.get in
    function
    | Env.Aident id -> (
        if Ident.global id then
          let globals = Lazy.force frame.globals in
          let pos = Ident.Map.find id globals in
          Scene.get_global scene pos
        else
          try%lwt
            let pos = Ident.find_same id event.ev_compenv.ce_stack in
            Scene.get_local scene frame (event.ev_stacksize - pos)
          with Not_found ->
            let pos = Ident.find_same id event.ev_compenv.ce_heap in
            Scene.get_environment scene frame pos )
    | Env.Adot (root, pos) ->
        let%lwt v = address scene frame path root in
        assert (Scene.is_block v);
        Scene.get_field scene v pos
  in
  let typenv = Lazy.force frame.Frame.typenv in
  let addr = typenv |> Typenv.find_value_address path in
  address scene frame path addr
