include Ocaml_common.Types

[%%if ocaml_version < (4, 14, 0)]
let get_desc ty = (Ctype.repr ty).desc
let commu_ok = Cok
let row_fields row = row.row_fields
let row_field_repr = Btype.row_field_repr
[%%endif]
