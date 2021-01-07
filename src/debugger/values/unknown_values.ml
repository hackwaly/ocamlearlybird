open Value_basic

module Unknown_value = struct
  type t += Unknown

  let extension_constructor = Obj.Extension_constructor.of_val Unknown

  let is_named_container = false

  let is_indexed_container = false

  let adopt conn env ty rv =
    ignore conn;
    ignore env;
    ignore ty;
    ignore rv;
    Lwt.return (Some Unknown)

  let to_short_string ?(hex = false) v =
    ignore hex;
    ignore v;
    "«unknown»"

  let num_indexed v =
    ignore v;
    0

  let get_indexed v index =
    ignore v;
    ignore index;
    [%lwt assert false]

  let num_named _ = 0

  let list_named v =
    ignore v;
    Lwt.return []
end
