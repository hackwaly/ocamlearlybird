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

type pc = int * int

module Sp = struct

  (* Position in the debuggee's stack. *)
  type t = {
    block : int;
    offset : int;
  }

  let null = { block = -1; offset = -1}

  let base sp n = {sp with offset = sp.offset - n}

  let compare sp1 sp2 =
    match Stdlib.compare sp1.block sp2.block with
    | 0 -> Stdlib.compare sp1.offset sp2.offset
    | x -> x

end

(* Identifier of the code fragment for the main program.
   Numbering starts at 1 and the runtime registers 2 fragments before
   the main program: one for uncaught exceptions and one for callbacks.
*)
let main_frag = 3

type 'a source_location = {
  source : string;
  pos : int * int;
  end_ : 'a;
}

type source_position = unit source_location

type source_range = (int * int) source_location
