open Debug_types
open Value_basic
open Value_simple
open Value_func

type raw_value = Scene.obj * Types.type_expr

type t =
  < to_short_string : string
  ; vscode_menu_context : string option
  ; closure_code_location : source_range option
  ; num_indexed : int
  ; num_named : int
  ; get_indexed : int -> t Lwt.t
  ; list_named : (string * t) list Lwt.t >

class opaque_block_value ~scene ~rv ~size =
  object
    inherit value
    method! num_indexed = size

    method! get_indexed (idx : int) : value Lwt.t =
      let%lwt fldval = Scene.get_field scene rv idx in
      dyn_adopt scene fldval

    method to_short_string = "«opaque block»"
  end

let () =
  (Value_basic.dyn_adopter :=
     fun scene obj ->
       let%lwt tag = Scene.get_tag scene obj in
       if tag = Obj.string_tag then
         let%lwt obj = Scene.marshal_obj scene obj in
         Lwt.return (new string_value (Obj.magic obj))
       else if tag = Obj.int_tag then
         let%lwt obj = Scene.marshal_obj scene obj in
         Lwt.return (new int_value (Obj.magic obj))
       else if tag = Obj.double_tag then
         let%lwt obj = Scene.marshal_obj scene obj in
         Lwt.return (new float_value (Obj.magic obj))
       else if tag = Obj.closure_tag then
         let%lwt pc, loc =
           if Scene.is_block obj then
             let%lwt pc, loc = Scene.get_closure_code scene obj in
             Lwt.return (Some pc, loc)
           else Lwt.return (None, None)
         in
         Lwt.return (new func_value ?pc ?loc ())
       else if tag = Obj.double_array_tag then
         (* TODO: *)
         Lwt.return unknown_value
       else if tag = Obj.lazy_tag then (* TODO: *)
         Lwt.return unknown_value
       else if Scene.is_block obj then
         let%lwt size = Scene.get_size scene obj in
         Lwt.return (new opaque_block_value ~scene ~rv:obj ~size)
       else Lwt.return unknown_value);

  Value_basic.adopters :=
    [
      Value_simple.adopter;
      Value_struct.adopter;
      Value_list.adopter;
      Value_array.adopter;
      Value_func.adopter;
      Value_module.adopter;
      Value_object.adopter;
      Value_lazy.adopter;
    ]

let scope scene frame kind =
  match kind with
  | `Stack ->
      (new Value_scope.local_scope_value ~scene ~frame ~kind:`Stack () :> t)
  | `Heap ->
      (new Value_scope.local_scope_value ~scene ~frame ~kind:`Heap () :> t)
  | `Global -> (new Value_scope.global_scope_value ~scene ~frame () :> t)
