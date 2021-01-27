(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Berke Durak *)
(* Propositional formulas. *)

type 'a t = And of 'a t list | Or of 'a t list | Not of 'a t | Atom of 'a | True | False;;

let rec eval f = function
  | And l -> List.for_all (eval f) l
  | Or l -> List.exists (eval f) l
  | Not x -> not (eval f x)
  | Atom a -> f a
  | True -> true
  | False -> false
;;
let rec iter f = function
  | (And l|Or l) -> List.iter (iter f) l
  | Not x -> iter f x
  | Atom a -> f a
  | True|False -> ()
;;
let rec map f = function
  | And l -> And(List.map (map f) l)
  | Or l -> Or(List.map (map f) l)
  | Not x -> Not(map f x)
  | Atom a -> Atom(f a)
  | (True|False) as b -> b
;;
