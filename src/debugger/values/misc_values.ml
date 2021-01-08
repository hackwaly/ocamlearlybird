open Value_basic

module Impl_base_value = struct
  let extension_constructor = Opaque

  let is_indexed_container = false

  let adopt _ _ _ _ = Lwt.return None

  let to_short_string ?(hex = false) _ =
    ignore hex;
    [%lwt assert false]

  let num_indexed _ = 0

  let num_named _ = 0

  let list_named _ = Lwt.return []

  let get_indexed _ _ = [%lwt assert false]
end

module Opaque_value = struct
  include Impl_base_value

  let extension_constructor = Obj.Extension_constructor.of_val Opaque

  let is_indexed_container = false

  let adopt _ _ _ _ = Lwt.return (Some Opaque)

  let to_short_string ?(hex = false) _ =
    ignore hex;
    "«opaque»"
end

module Raw_string_value = struct
  include Impl_base_value

  type t += Raw_string of string

  let extension_constructor =
    Obj.Extension_constructor.of_val (Raw_string (Obj.magic ()))

  let to_short_string ?(hex = false) v =
    ignore hex;
    let[@warning "-8"] (Raw_string str) = (v [@warning "+8"]) in
    str
end

module Abstract_value = struct
  include Impl_base_value

  type t += Abstract of Path.t

  let extension_constructor =
    Obj.Extension_constructor.of_val (Abstract (Obj.magic ()))

  let to_short_string ?(hex = false) v =
    ignore hex;
    let[@warning "-8"] (Abstract path) = (v [@warning "+8"]) in
    Util.Path.to_string path

  let adopt conn env ty rv =
    ignore conn;
    ignore rv;
    match (Ctype.repr ty).desc with
    | Types.Tconstr (path, _, _)
      when match Env.find_type path env with
           | exception Not_found -> false
           | { type_kind = Type_abstract; _ } -> true
           | _ -> false ->
        Lwt.return (Some (Abstract path))
    | _ -> Lwt.return None
end
