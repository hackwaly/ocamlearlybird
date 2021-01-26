open Ground
open Value_basic
open Frame

class virtual scope_value scene frame =
  object (self)
    inherit value

    method to_short_string = "«scope»"

    method virtual get_slot : int -> Scene.obj Lwt.t

    method virtual variables : (string * Types.type_expr * int) array

    method! num_named = Array.length self#variables

    method! list_named =
      let typenv = Lazy.force frame.typenv in
      self#variables |> Array.to_seq |> List.of_seq
      |> Lwt_list.map_s (fun (id, ty, pos) ->
             let%lwt rv = self#get_slot pos in
             let%lwt obj = adopt scene typenv rv ty in
             Lwt.return (id, obj))
  end

class local_scope_value scene frame =
  let variables =
    Lazy.from_fun (fun () ->
        match frame.event with
        | None -> [||]
        | Some event ->
            let typenv = Lazy.force frame.typenv in
            let compenv = event.ev_compenv.ce_stack in
            let iter f = compenv |> Ident.iter (fun id pos -> f (id, pos)) in
            Iter.to_list iter
            |> List.fast_sort (Compare.by (fun (_, pos) -> pos))
            |> List.to_seq
            |> Seq.filter_map (fun (id, pos) ->
                   match typenv |> Typenv.find_value (Path.Pident id) with
                   | exception Not_found -> None
                   | { val_type; _ } ->
                       let ty = Ctype.correct_levels val_type in
                       Some (Ident.name id, ty, pos))
            |> Array.of_seq)
  in
  object
    inherit scope_value scene frame

    method get_slot i = Scene.get_local scene frame i

    method variables = Lazy.force variables
  end
