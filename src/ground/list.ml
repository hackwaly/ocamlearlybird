(*
 * BatList - additional and modified functions for lists.
 * Copyright (C) 2003 Brian Hurt
 * Copyright (C) 2003 Nicolas Cannasse
 * Copyright (C) 2008 Red Hat Inc.
 * Copyright (C) 2008 David Rajchenbach-Teller, LIFO, Universite d'Orleans
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Stdlib
include List

(* CREDIT: Copied from batteries BatList.mlv *)
type 'a mut_list = { hd : 'a; mutable tl : 'a list }

external inj : 'a mut_list -> 'a list = "%identity"

module Acc = struct
  let dummy () = { hd = Obj.magic (); tl = [] }

  let create x = { hd = x; tl = [] }

  let accum acc x =
    let cell = create x in
    acc.tl <- inj cell;
    cell
end

let span p li =
  let rec loop dst = function
    | [] -> []
    | x :: xs as l -> if p x then loop (Acc.accum dst x) xs else l
  in
  let dummy = Acc.dummy () in
  let xs = loop dummy li in
  (dummy.tl, xs)

let group_consecutive p l =
  let rec loop dst = function
    | [] -> ()
    | x :: rest ->
        let xs, rest = span (p x) rest in
        loop (Acc.accum dst (x :: xs)) rest
  in
  let dummy = Acc.dummy () in
  loop dummy l;
  dummy.tl

let uniq_cons x xs = if List.mem x xs then xs else x :: xs

let uniq xs = List.fold_right uniq_cons xs []
