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

module Debug_event = struct
  open Instruct

  type t = debug_event

  let lex_pos t =
    match t.ev_kind with
    | Event_after _ -> t.ev_loc.loc_end
    | _ -> t.ev_loc.loc_start

  let cnum t = (lex_pos t).pos_cnum

  let line_column t = lex_pos t |> Lexing.Position.line_column
end

module Path = struct
  open Path

  let to_string path =
    let rec aux path =
      match path with
      | Pident id -> Ident.name id
      | Pdot (p, d) -> aux p ^ "." ^ d
      | Papply (p1, p2) -> aux p1 ^ " (" ^ aux p2 ^ ")"
    in
    aux path

  let rec to_longident path =
    match path with
    | Pident id -> Longident.Lident (Ident.name id)
    | Pdot (p, d) -> Longident.Ldot (to_longident p, d)
    | Papply (p1, p2) -> Longident.Lapply (to_longident p1, to_longident p2)
end
