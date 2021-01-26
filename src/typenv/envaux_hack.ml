(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Env_hack

type error =
    Module_not_found of Path.t

exception Error of error

let env_cache =
  (Hashtbl.create 59 : ((Env_hack.summary * Subst.t), Env_hack.t) Hashtbl.t)

let reset_cache () =
  Hashtbl.clear env_cache;
  Env_hack.reset_cache()

let rec env_from_summary sum subst =
  try
    Hashtbl.find env_cache (sum, subst)
  with Not_found ->
    let env =
      match sum with
        Env_empty ->
          Env_hack.empty
      | Env_value(s, id, desc) ->
          Env_hack.add_value id (Subst.value_description subst desc)
                        (env_from_summary s subst)
      | Env_type(s, id, desc) ->
          Env_hack.add_type ~check:false id
            (Subst.type_declaration subst desc)
            (env_from_summary s subst)
      | Env_extension(s, id, desc) ->
          Env_hack.add_extension ~check:false ~rebind:false id
            (Subst.extension_constructor subst desc)
            (env_from_summary s subst)
      | Env_module(s, id, pres, desc) ->
          Env_hack.add_module_declaration ~check:false id pres
            (Subst.module_declaration Keep subst desc)
            (env_from_summary s subst)
      | Env_modtype(s, id, desc) ->
          Env_hack.add_modtype id (Subst.modtype_declaration Keep subst desc)
                          (env_from_summary s subst)
      | Env_class(s, id, desc) ->
          Env_hack.add_class id (Subst.class_declaration subst desc)
                        (env_from_summary s subst)
      | Env_cltype (s, id, desc) ->
          Env_hack.add_cltype id (Subst.cltype_declaration subst desc)
                         (env_from_summary s subst)
      | Env_open(s, path) ->
          let env = env_from_summary s subst in
          let path' = Subst.module_path subst path in
          begin match Env_hack.open_signature Asttypes.Override path' env with
          | Ok env -> env
          | Error `Functor -> assert false
          | Error `Not_found -> raise (Error (Module_not_found path'))
          end
      | Env_functor_arg(Env_module(s, id, pres, desc), id')
            when Ident.same id id' ->
          Env_hack.add_module_declaration ~check:false
            id pres (Subst.module_declaration Keep subst desc)
            ~arg:true (env_from_summary s subst)
      | Env_functor_arg _ -> assert false
      | Env_constraints(s, map) ->
          Path.Map.fold
            (fun path info ->
              Env_hack.add_local_type (Subst.type_path subst path)
                (Subst.type_declaration subst info))
            map (env_from_summary s subst)
      | Env_copy_types s ->
          let env = env_from_summary s subst in
          Env_hack.make_copy_of_types env env
      | Env_persistent (s, id) ->
          let env = env_from_summary s subst in
          Env_hack.add_persistent_structure id env
      | Env_value_unbound (s, str, reason) ->
          let env = env_from_summary s subst in
          Env_hack.enter_unbound_value str reason env
      | Env_module_unbound (s, str, reason) ->
          let env = env_from_summary s subst in
          Env_hack.enter_unbound_module str reason env
    in
      Hashtbl.add env_cache (sum, subst) env;
      env

let env_of_only_summary env =
  Env_hack.env_of_only_summary env_from_summary env

(* Error report *)

open Format

let report_error ppf = function
  | Module_not_found p ->
      fprintf ppf "@[Cannot find module %a@].@." Printtyp.path p

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

(* HACK *)
let () = Env_hack.strengthen :=
  (fun ~aliasable env mty path -> Mtype.strengthen ~aliasable (Obj.magic env) mty path)
(* /HACK *)
