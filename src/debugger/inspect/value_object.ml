open Value_basic

class object_value ~scene ~typenv ~obj ~members () =
  object
    inherit value

    method to_short_string = "«object»"

    method! num_named = List.length members

    method! list_named =
      let%lwt table = Scene.get_field scene obj 0 in
      let%lwt num_methods = Scene.get_field scene table 0 in
      let%lwt num_methods = Scene.marshal_obj scene num_methods in
      let find_method name =
        let left = ref 0 in
        let right = ref num_methods in
        let tag = CamlinternalOO.public_method_label name in
        while%lwt !left < !right do
          let middle = (!left + !right) / 2 in
          let%lwt tag' = Scene.get_field scene table (2 + (2 * middle) + 1) in
          let%lwt tag' = Scene.marshal_obj scene tag' in
          if tag <= tag' then right := middle else left := middle + 1;
          Lwt.return ()
        done;%lwt
        let%lwt method' = Scene.get_field scene table (2 + (2 * !left)) in
        Lwt.return method'
      in
      members
      |> Lwt_list.map_s (fun (name, ty) ->
             let ty =
               Ctype.newty (Types.Tarrow (Nolabel, Predef.type_unit, ty, Cok))
             in
             let%lwt meth = find_method name in
             let%lwt value = adopt scene typenv meth ty in
             Lwt.return (name, value))
  end

let adopter scene typenv obj ty =
  match (Ctype.repr ty).desc with
  | Tobject (fields_ty, _) ->
      let rec aux r fields_ty =
        match fields_ty.Types.desc with
        | Types.Tfield (name, _, ty', fields_ty) ->
            aux ((name, ty') :: r) fields_ty
        | Tnil -> r
        | _ -> assert false
      in
      let members = aux [] fields_ty in
      Lwt.return (Some (new object_value ~scene ~typenv ~obj ~members ()))
  | _ -> Lwt.return None
