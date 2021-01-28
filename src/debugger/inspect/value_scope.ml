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
open Value_basic
open Frame

class virtual scope_value =
  object
    inherit value

    method to_short_string = "«scope»"

    method! num_named = -1
  end

class local_scope_value ~scene ~frame ~kind () =
  let variables =
    Lazy.from_fun (fun () ->
        match frame.event with
        | None -> [||]
        | Some event ->
            let typenv = Lazy.force frame.typenv in
            let compenv =
              match kind with
              | `Stack -> event.ev_compenv.ce_stack
              | `Heap -> event.ev_compenv.ce_heap
            in
            let iter f = compenv |> Ident.iter (fun id pos -> f (id, pos)) in
            Iter.to_list iter
            |> List.fast_sort (Compare.by (fun (_, pos) -> pos))
            |> List.to_seq
            |> Seq.filter_map (fun (id, pos) ->
                   match typenv |> Typenv.find_value (Path.Pident id) with
                   | exception Not_found -> None
                   | { val_type; val_kind; _ } ->
                       let ty = Ctype.correct_levels val_type in
                       Some (Ident.name id, val_kind, ty, pos))
            |> Array.of_seq)
  in
  object
    inherit scope_value

    method! num_named = Lazy.force variables |> Array.length

    method! list_named =
      let typenv = Lazy.force frame.typenv in
      Lazy.force variables |> Array.to_list
      |> Lwt_list.map_s (fun (id, val_kind, ty, pos) ->
             let%lwt rv =
               match kind with
               | `Stack -> Scene.get_local scene frame pos
               | `Heap -> Scene.get_environment scene frame pos
             in
             match val_kind with
             | Types.Val_ivar (_, cl_num) -> (
                 try
                   let p0, _ =
                     Typenv.find_value_by_name
                       (Longident.Lident ("self-" ^ cl_num))
                       typenv
                   in
                   let%lwt v = Eval.value_path scene frame p0 in
                   let%lwt i = Scene.marshal_obj scene rv in
                   let%lwt rv' = Scene.get_field scene v i in
                   let%lwt obj = adopt scene typenv rv' ty in
                   Lwt.return (id, obj)
                 with _ -> Lwt.return (id, uninitialized_value) )
             | _ ->
                 let%lwt obj = adopt scene typenv rv ty in
                 Lwt.return (id, obj))
  end

class global_scope_value ~scene ~frame () =
  object
    inherit scope_value

    method! list_named =
      let typenv = Lazy.force frame.typenv in
      Lazy.force frame.globals |> Ident.Map.to_seq
      |> Seq.filter (fun (name, _) ->
             try typenv |> Typenv.is_structure_module (Path.Pident name)
             with _ -> false)
      |> List.of_seq
      |> Lwt_list.filter_map_s (fun (id, pos) ->
             let%lwt obj = Scene.get_global scene pos in
             if Scene.is_block obj then
               Lwt.return
                 (Some
                    ( Ident.name id,
                      new Value_module.module_value
                        ~scene ~typenv ~obj ~path:(Path.Pident id) () ))
             else Lwt.return None)
  end
