include Ocaml_common.Ctype

[%%if ocaml_version < (5, 3, 0)]
let duplicate_type = correct_levels
[%%endif]
