open Value_basic

module Impl_base_value = struct
  let extension_constructor = Unknown

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

module Unknown_value = struct
  include Impl_base_value

  let extension_constructor = Obj.Extension_constructor.of_val Unknown

  let is_indexed_container = false

  let adopt _ _ _ _ =
    Lwt.return (Some Unknown)

  let to_short_string ?(hex = false) _ =
    ignore hex;
    "«unknown»"
end

module Raw_string_value = struct
  include Impl_base_value

  type t += Raw_string of string

  let extension_constructor =
    Obj.Extension_constructor.of_val (Raw_string (Obj.magic ()))

  let to_short_string ?(hex=false) v =
    ignore hex;
    let[@warning "-8"] (Raw_string str) = (v [@warning "+8"]) in
    str
end
