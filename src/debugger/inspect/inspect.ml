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

type raw_value = Scene.obj * Types.type_expr

type t =
  < to_short_string : string
  ; num_indexed : int
  ; num_named : int
  ; get_indexed : int -> t Lwt.t
  ; list_named : (string * t) list Lwt.t >

let () =
  Value_basic.adopters :=
    [
      Value_simple.adopter;
      Value_struct.adopter;
      Value_list.adopter;
      Value_array.adopter;
      Value_func.adopter;
      Value_module.adopter;
      Value_object.adopter;
      Value_lazy.adopter;
    ]

let scope scene frame kind =
  match kind with
  | `Stack -> (new Value_scope.local_scope_value ~scene ~frame ~kind:`Stack () :> t)
  | `Heap -> (new Value_scope.local_scope_value ~scene ~frame ~kind:`Heap () :> t)
  | `Global -> (new Value_scope.global_scope_value ~scene ~frame () :> t)
