type t = Debuginfo.event = {
  module_ : Debuginfo.module_;
  ev : Instruct.debug_event;
  env : Env.t Lwt.t Lazy.t;
}

let pc ev = { Pc.frag = ev.module_.frag; pos = ev.ev.ev_pos }

let find_next_event prev =
  let module_ = prev.module_ in
  let source = module_.source |> Option.get in
  let%lwt structure = Util.parse_impl source in
  let module E = struct
    exception Found_next_cnum of int

    exception Found_expr of Parsetree.expression

    exception Found_after_expr of Parsetree.expression

    type ctx = { cur_cnum : int }

    let rec iter_expr ctx it e =
      if e.Parsetree.pexp_loc.loc_start.pos_cnum = ctx.cur_cnum then
        raise (Found_expr e)
      else if e.Parsetree.pexp_loc.loc_end.pos_cnum = ctx.cur_cnum then
        raise (Found_after_expr e)
      else
        match e.Parsetree.pexp_desc with
        | Pexp_sequence (e1, e2)
          when e1.pexp_loc.loc_end.pos_cnum = ctx.cur_cnum ->
            raise (Found_next_cnum e2.pexp_loc.loc_start.pos_cnum)
        | Pexp_sequence (e1, e2) ->
            ( try it.Ast_iterator.expr it e1 with
            | Found_after_expr e
              when e.pexp_loc.loc_end.pos_cnum = e1.pexp_loc.loc_end.pos_cnum ->
                raise (Found_expr e)
            | Found_expr _ | Found_after_expr _ ->
                raise (Found_next_cnum e2.pexp_loc.loc_start.pos_cnum) );
            it.expr it e2
        | Pexp_let (Nonrecursive, bindings, e2) ->
            ( try List.iter (fun binding -> it.value_binding it binding) bindings
              with Found_expr _ ->
                raise (Found_next_cnum e2.pexp_loc.loc_start.pos_cnum) );
            it.expr it e2
        | _ -> Ast_iterator.default_iterator.expr it e
  end in
  let open E in
  let it =
    {
      Ast_iterator.default_iterator with
      expr =
        (let ctx =
           { cur_cnum = (Util.Debug_event.lexing_position prev.ev).pos_cnum }
         in
         let it' =
           { Ast_iterator.default_iterator with expr = E.iter_expr ctx }
         in
         fun it e ->
           try it'.expr it e with
           | Found_after_expr e'
             when e'.pexp_loc.loc_end.pos_cnum = e.pexp_loc.loc_end.pos_cnum ->
               raise Not_found
           | Found_expr _ | Found_after_expr _ ->
               raise (Found_next_cnum e.pexp_loc.loc_end.pos_cnum));
    }
  in
  let next_cnum =
    try
      it.structure it structure;
      raise Not_found
    with Found_next_cnum t -> t
  in
  let event = Module.find_event_by_cnum module_ next_cnum in
  Lwt.return event
