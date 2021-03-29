type t

val from_summary :
  get_search_dirs:(string -> string list) ->
  Env.summary -> Subst.t -> t
val find_value : Path.t -> t -> Types.value_description
val find_value_by_name : Longident.t -> t -> Path.t * Types.value_description
val find_type : Path.t -> t -> Types.type_declaration
val find_constructor_by_name :
  Longident.t -> t -> Types.constructor_description
val find_value_address : Path.t -> t -> Env.address
val find_module_address : Path.t -> t -> Env.address
val find_modtype_expansion : Path.t -> t -> Types.module_type
val is_structure_module : Path.t -> t -> bool
val add_module :
  ?arg:bool ->
  Ident.t -> Types.module_presence -> Types.module_type -> t -> t
val extract_modules : Longident.t option -> t -> string list
val extract_values : Longident.t option -> t -> string list
val type_apply :
  t ->
  Types.type_expr list ->
  Types.type_expr -> Types.type_expr list -> Types.type_expr
val type_matches : t -> Types.type_expr -> Types.type_expr -> bool
val full_expand : t -> Types.type_expr -> Types.type_expr
