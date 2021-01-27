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

(** A lexer function for globbing formulas.

   To parse globbing formulas you should rather use
   the{!Path_glob.Glob.parse} function.

   The lexer provides a lower-level access that might be combined with
   other lexers in a larger grammar.
*)

(* Original author: Berke Durak *)
open Ast

type token =
| ATOM of pattern atom
| AND
| OR
| NOT
| LPAR
| RPAR
| TRUE
| FALSE
| EOF

val token : Lexing.lexbuf -> token
