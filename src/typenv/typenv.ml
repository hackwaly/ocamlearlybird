let persistent_env_get_search_dirs = ref ((fun _ -> assert false) : string -> string list)

[%%if ocaml_version >= (5, 0, 0)]
let load_path_init = Load_path.init ~auto_include:Load_path.no_auto_include
[%%else]
let load_path_init = Load_path.init
[%%endif]

let () =
  let old_load = !Persistent_env.Persistent_signature.load in
  Persistent_env.Persistent_signature.load := (fun ~unit_name ->
      let search_dirs = !persistent_env_get_search_dirs unit_name in
      load_path_init search_dirs;
      old_load ~unit_name
    )

let env_extract_values path env =
  Env.fold_values (fun name _ _ acc -> name :: acc) path env []

let env_extract_modules path env =
  Env.fold_modules (fun name _ _ acc -> name :: acc) path env []

let env_is_structure_module path env =
  match Env.find_module path env with
  | {md_type; _} ->
    begin match Env.scrape_alias env md_type with
      | Mty_signature _ -> true
      | Mty_functor _ -> false
      | Mty_alias _ | Mty_ident _ -> failwith "scrape_alias didn't work"
    end
  | exception Not_found -> false


 type t = { env : Env.t; get_search_dirs : string -> string list }

 let from_summary ~get_search_dirs summary subst =
   persistent_env_get_search_dirs := get_search_dirs;
   Envaux.reset_cache ();
   let env = Envaux.env_from_summary summary subst in
   { env; get_search_dirs }

 let find_value path { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   Env.find_value path env

 let find_value_by_name name { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   Env.find_value_by_name name env

 let find_type path { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   Env.find_type path env

 let find_constructor_by_name lid { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   Env.find_constructor_by_name lid env

 let find_value_address path { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   Env.find_value_address path env

 let find_module_address path { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   Env.find_module_address path env

 let find_modtype_expansion path { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   Env.find_modtype_expansion path env

 let is_structure_module path { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   env_is_structure_module path env

 let add_module ?arg id presence mty { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   let env' = Env.add_module ?arg id presence mty env in
   { env = env'; get_search_dirs }

 let extract_modules path { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   env_extract_modules path env

 let extract_values path { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   env_extract_values path env

 let type_apply { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   Ctype.apply env

[%%if ocaml_version < (4, 13, 0)]
 let type_matches { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   Ctype.matches env
[%%else]
 let type_matches { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   Ctype.does_match env
[%%endif]

[%%if ocaml_version < (4, 13, 0)]
 let full_expand { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   Ctype.full_expand env
[%%else]
 let full_expand { env; get_search_dirs } =
   persistent_env_get_search_dirs := get_search_dirs;
   Ctype.full_expand ~may_forget_scope:false env
[%%endif]
