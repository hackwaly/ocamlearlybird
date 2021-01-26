type raw_value = Scene.obj * Types.type_expr

type t =
  < to_short_string : string
  ; num_indexed : int
  ; num_named : int
  ; get_indexed : int -> t Lwt.t
  ; list_named : (string * t) list Lwt.t >

let () = Value_basic.adopters := [ Value_simple.adopter ]

let local_scope scene frame =
  (new Value_scope.local_scope_value scene frame :> t)
