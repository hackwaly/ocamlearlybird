type t = { env : Env_hack.t; get_search_dirs : string -> string list }

let from_summary ~get_search_dirs summary subst =
  Envaux_hack.reset_cache ();
  let env = Envaux_hack.env_from_summary summary subst in
  { env; get_search_dirs }

let find_value path { env; get_search_dirs } =
  Persistent_env_hack.get_search_dirs := get_search_dirs;
  Env_hack.find_value path env

let find_type path { env; get_search_dirs } =
  Persistent_env_hack.get_search_dirs := get_search_dirs;
  Env_hack.find_type path env

let find_constructor_by_name lid { env; get_search_dirs } =
  Persistent_env_hack.get_search_dirs := get_search_dirs;
  Env_hack.find_constructor_by_name lid env

let find_value_address path { env; get_search_dirs } =
  Persistent_env_hack.get_search_dirs := get_search_dirs;
  Env_hack.find_value_address path env

let find_module_address path { env; get_search_dirs } =
  Persistent_env_hack.get_search_dirs := get_search_dirs;
  Env_hack.find_module_address path env

let type_apply {env; get_search_dirs} =
  Persistent_env_hack.get_search_dirs := get_search_dirs;
  Ctype_hack.apply (Obj.magic env)

let type_matches {env; get_search_dirs} =
  Persistent_env_hack.get_search_dirs := get_search_dirs;
  Ctype_hack.matches (Obj.magic env)
