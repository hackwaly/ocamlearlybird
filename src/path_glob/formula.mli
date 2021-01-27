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
(* Formula *)

(** Provides a datatype for representing propositional formulas and evaluation,
    iteration and map functions. *)

(** Public type for generic propositional formulas.
    An empty conjunction [And[]] is true and
    an empty disjunction [Or[]] is false. *)
type 'a t =
    And of 'a t list
  | Or of 'a t list
  | Not of 'a t
  | Atom of 'a
  | True
  | False

val eval : ('a -> bool) -> 'a t -> bool
(** [eval g f] evaluates the formula [f] using the values returned by [g] for the atoms. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter g f] calls [g] over every atom of [f]. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map g f] replaces every atom of [f] by its image by [g]. *)
