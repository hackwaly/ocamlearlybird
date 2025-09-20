open Ground

module Debug_event = struct
  open Instruct

  type t = debug_event

  let lex_pos t =
    match t.ev_kind with
    | Event_after _ -> t.ev_loc.loc_end
    | _ -> t.ev_loc.loc_start

  let cnum t = (lex_pos t).pos_cnum

  let line_column t = lex_pos t |> Lexing.Position.line_column
end

module Path = struct
  open Path

  let to_string path =
    let rec aux path =
      match path with
      | Pident id -> Ident.name id
      | Pdot (p, d) -> aux p ^ "." ^ d
      | Pextra_ty (p, Pcstr_ty d) [@if ocaml_version >= (5, 1, 0)] -> aux p ^ "." ^ d
      | Papply (p1, p2) -> aux p1 ^ " (" ^ aux p2 ^ ")"
      | Pextra_ty (p, Pext_ty) [@if ocaml_version >= (5, 1, 0)] -> aux p
    in
    aux path

  [%%if ocaml_version >= (5, 4, 0)]
  let lift_longident = Location.mknoloc
  [%%else]
  let lift_longident = Fun.id
  [%%endif]

  let rec to_longident path =
    match path with
    | Pident id -> Longident.Lident (Ident.name id)
    | Pdot (p, d) -> Longident.Ldot (lift_longident (to_longident p), lift_longident d)
    | Pextra_ty (p, Pcstr_ty d) [@if ocaml_version >= (5, 1, 0)] -> Longident.Ldot (lift_longident (to_longident p), lift_longident d)
    | Papply (p1, p2) -> Longident.Lapply (lift_longident (to_longident p1), lift_longident (to_longident p2))
    | Pextra_ty (p, Pext_ty) [@if ocaml_version >= (5, 1, 0)] -> to_longident p
end
