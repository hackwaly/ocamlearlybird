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
(* Glob *)

(** A globber is a boolean combination of basic expressions indented to work on
    pathnames.  Known operators
    are [or], [and] and [not], which may also be written [|], [&] and [~].  There are
    also constants [true] and [false] (or [1] and [0]).  Expression can be grouped
    using parentheses.
    - [true] matches anything,
    - [false] matches nothing,
    - {i basic} [or] {i basic} matches strings matching either one of the basic expressions,
    - {i basic} [and] {i basic} matches strings matching both basic expressions,
    - [not] {i basic} matches string that don't match the basic expression,
    - {i basic} matches strings that match the basic expression.

    A basic expression can be a constant string enclosed in double quotes, in which
    double quotes must be preceded by backslashes, or a glob pattern enclosed between a [<] and a [>],
    - ["]{i string}["] matches the literal string {i string},
    - [<]{i glob}[>] matches the glob pattern {i glob}.

    A glob pattern is an anchored regular expression in a shell-like syntax.  Most characters stand for themselves.
    Character ranges are given in usual shell syntax between brackets.  The star [*] stands for any sequence of
    characters.  The joker '?' stands for exactly one, unspecified character.  Alternation is achieved using braces [{].
    - {i glob1}{i glob2} matches strings who have a prefix matching {i glob1} and the corresponding suffix
      matching {i glob2}.
    - [a] matches the string consisting of the single letter [a].
    - [{]{i glob1},{i glob2}[}] matches strings matching {i glob1} or {i glob2}.
    - [?] matches any one-letter string, excluding the slash.
    - [*] matches all strings not containing a slash, including the empty one.
    - [**/] matches the empty string, or any string ending with a slash.
    - [/**] matches any string starting with a slash, or the empty string.
    - [/**/] matches any string starting and ending with a slash.
    - [\[]{i c1}-{i c2}{i c3}-{i c4}...[\]] matches characters in the range {i c1} to {i c2} inclusive,
      or in the range {i c3} to {i c4} inclusive.  For instance [\[a-fA-F0-9\]] matches hexadecimal digits.
      To match the dash, put it at the end.
*)

(** The type representing fast patterns. Do not attempt to compare
   them, as they get on-the-fly optimizations. *)
type fast_pattern

val fast_pattern_of_pattern : Ast.pattern -> fast_pattern

(** The type representing globbers.  Do not attempt to compare them,
   as they get on-the-fly optimizations. *)
type globber = fast_pattern Ast.atom Formula.t

(** [parse ~dir pattern] will parse the globber pattern [pattern],
   optionally prefixing its patterns with [dir]. *)
val parse : ?dir:string -> string -> globber

(** A descriptive exception raised when an invalid glob pattern description is given. *)
exception Parse_error of string

(** [eval g u] returns [true] if and only if the string [u] matches
   the given glob expression. Avoid re-parsing the same pattern,
   since the automaton implementing the pattern is optimized on the
   fly. The first few evaluations are done using a time-inefficient
   but memory-efficient algorithm. It then compiles the pattern into
   an efficient but more memory-hungry data structure. *)
val eval : globber -> string -> bool
