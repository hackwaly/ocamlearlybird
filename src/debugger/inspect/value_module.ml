open Ground
open Value_basic

let unknown_module_value =
  object
    inherit value

    method to_short_string = "«module»"
  end

class module_value ~scene ~typenv ~obj ~path () =
  object
    inherit value

    method to_short_string = "«module»"

    method! num_named = -1

    method! list_named =
      let lid = Util.Path.to_longident path in
      let get_pos addr =
        match addr with Env.Adot (_, pos) -> Some pos | _ -> None
      in
      let%lwt modules =
        typenv
        |> Typenv.extract_modules (Some lid)
        |> List.to_seq
        |> Seq.map (fun name -> Path.Pdot (path, name))
        |> Seq.filter (fun path -> typenv |> Typenv.is_structure_module path)
        |> Seq.filter_map (fun path ->
               try
                 let open Option in
                 let addr = typenv |> Typenv.find_module_address path in
                 let* pos = get_pos addr in
                 return (path, pos)
               with Not_found -> None)
        |> List.of_seq
        |> Lwt_list.map_s (fun (path, pos) ->
               let%lwt obj' = Scene.get_field scene obj pos in
               Lwt.return
                 ( Path.last path,
                   new module_value ~scene ~typenv ~obj:obj' ~path () ))
      in
      let%lwt values =
        typenv
        |> Typenv.extract_values (Some lid)
        |> List.to_seq
        |> Seq.map (fun name -> Path.Pdot (path, name))
        |> Seq.filter_map (fun path ->
               try
                 let open Option in
                 let valdesc = typenv |> Typenv.find_value path in
                 let addr = typenv |> Typenv.find_value_address path in
                 let* pos = get_pos addr in
                 return (path, valdesc.val_type, pos)
               with Not_found -> None)
        |> List.of_seq
        |> Lwt_list.map_s (fun (path, typ, pos) ->
               let%lwt obj' = Scene.get_field scene obj pos in
               let%lwt value = adopt scene typenv obj' typ in
               Lwt.return (Path.last path, value))
      in
      Lwt.return (modules @ values)
  end

let adopter scene typenv obj typ =
  match (Ctype.repr typ).desc with
  | Tpackage (path, [], []) -> (
      try
        let mty = typenv |> Typenv.find_modtype_expansion path in
        let mid =
          Ident.create_persistent
            (Printf.sprintf "M_%04x"
                (Float.to_int (Sys.time () *. 1e9) mod 0x10000))
        in
        let typenv' = typenv |> Typenv.add_module mid Types.Mp_present mty in
        Lwt.return
          (Some (new module_value ~scene ~typenv:typenv' ~obj ~path:(Path.Pident mid) ()))
      with _ -> Lwt.return (Some unknown_module_value) )
  | _ -> Lwt.return None
