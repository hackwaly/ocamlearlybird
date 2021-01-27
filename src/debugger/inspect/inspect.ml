type raw_value = Scene.obj * Types.type_expr

type t =
  < to_short_string : string
  ; num_indexed : int
  ; num_named : int
  ; get_indexed : int -> t Lwt.t
  ; list_named : (string * t) list Lwt.t >

let () =
  Value_basic.adopters :=
    [
      Value_simple.adopter;
      Value_struct.adopter;
      Value_list.adopter;
      Value_array.adopter;
      Value_func.adopter;
      Value_module.adopter;
    ]

let scope scene frame kind =
  match kind with
  | `Stack -> (new Value_scope.local_scope_value ~scene ~frame ~kind:`Stack () :> t)
  | `Heap -> (new Value_scope.local_scope_value ~scene ~frame ~kind:`Heap () :> t)
  | `Global -> (new Value_scope.global_scope_value ~scene ~frame () :> t)
