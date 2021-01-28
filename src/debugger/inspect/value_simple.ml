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

class int_value v =
  object
    inherit value

    method to_short_string = Int.to_string v
  end

class char_value v =
  object
    inherit value

    method to_short_string = "'" ^ Char.escaped v ^ "'"
  end

class string_value v =
  object
    inherit value

    method to_short_string = "\"" ^ String.escaped v ^ "\""
  end

class bytes_value v =
  object
    inherit value

    method to_short_string = "«bytes»"

    method! num_indexed = Bytes.length v

    method! get_indexed i = Lwt.return (new int_value (Bytes.get_uint8 v i))
  end

class float_value v =
  object
    inherit value

    method to_short_string = Float.to_string v
  end

class bool_value v =
  object
    inherit value

    method to_short_string = Bool.to_string v
  end

let unit_value =
  object
    inherit value

    method to_short_string = "()"
  end

class nativeint_value v =
  object
    inherit value

    method to_short_string = Nativeint.to_string v
  end

class int32_value v =
  object
    inherit value

    method to_short_string = Int32.to_string v
  end

class int64_value v =
  object
    inherit value

    method to_short_string = Int64.to_string v
  end

class extension_constructor_value v =
  object
    inherit value

    method to_short_string = Obj.Extension_constructor.name v
  end

let adopter scene typenv obj typ =
  if Typenv.type_matches typenv Predef.type_int typ then
    let%lwt obj = Scene.marshal_obj scene obj in
    Lwt.return (Some (new int_value (Obj.magic obj)))
  else if Typenv.type_matches typenv Predef.type_char typ then
    let%lwt obj = Scene.marshal_obj scene obj in
    Lwt.return (Some (new char_value (Obj.magic obj)))
  else if Typenv.type_matches typenv Predef.type_string typ then
    let%lwt obj = Scene.marshal_obj scene obj in
    Lwt.return (Some (new string_value (Obj.magic obj)))
  else if Typenv.type_matches typenv Predef.type_bytes typ then
    let%lwt obj = Scene.marshal_obj scene obj in
    Lwt.return (Some (new bytes_value (Obj.magic obj)))
  else if Typenv.type_matches typenv Predef.type_float typ then
    let%lwt obj = Scene.marshal_obj scene obj in
    Lwt.return (Some (new float_value (Obj.magic obj)))
  else if Typenv.type_matches typenv Predef.type_bool typ then
    let%lwt obj = Scene.marshal_obj scene obj in
    Lwt.return (Some (new bool_value (Obj.magic obj)))
  else if Typenv.type_matches typenv Predef.type_unit typ then
    Lwt.return (Some unit_value)
  else if Typenv.type_matches typenv Predef.type_nativeint typ then
    let%lwt obj = Scene.marshal_obj scene obj in
    Lwt.return (Some (new nativeint_value (Obj.magic obj)))
  else if Typenv.type_matches typenv Predef.type_int32 typ then
    let%lwt obj = Scene.marshal_obj scene obj in
    Lwt.return (Some (new int32_value (Obj.magic obj)))
  else if Typenv.type_matches typenv Predef.type_int64 typ then
    let%lwt obj = Scene.marshal_obj scene obj in
    Lwt.return (Some (new int64_value (Obj.magic obj)))
  else if Typenv.type_matches typenv Predef.type_extension_constructor typ then
    let%lwt obj = Scene.marshal_obj scene obj in
    Lwt.return (Some (new extension_constructor_value (Obj.magic obj)))
  else Lwt.return None
