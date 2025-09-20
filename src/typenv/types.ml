include Ocaml_common.Types

[%%if ocaml_version >= (5, 4, 0)]
type constructor_description = Data_types.constructor_description
[%%endif]
