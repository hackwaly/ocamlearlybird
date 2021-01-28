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

{
open Trivia_parser
}

rule comment = parse
  | "*)" {}
  | "(*" { comment lexbuf; comment lexbuf }
  | _+ { comment lexbuf }

and token = parse
  | eof { EOF }
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | ";" { SEMICOLON }
  | "(*)" { token lexbuf }
  | "(*" { comment lexbuf; token lexbuf }
  | "in" { IN }
  | _ { failwith "" }
