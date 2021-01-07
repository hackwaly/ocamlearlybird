include Value_basic
open Unknown_values
open Simple_values
open Lazy_values
open List_values
open Array_values
open Tuple_values
open Record_values
open Func_values

let modules =
  [
    (module Int_value : VALUE);
    (module Char_value : VALUE);
    (module String_value : VALUE);
    (module Bytes_value : VALUE);
    (module Float_value : VALUE);
    (module Bool_value : VALUE);
    (module Unit_value : VALUE);
    (module Nativeint_value : VALUE);
    (module Int32_value : VALUE);
    (module Int64_value : VALUE);
    (module Extension_constructor_value : VALUE);
    (module Tuple_value : VALUE);
    (module List_cons_value : VALUE);
    (module List_nil_value : VALUE);
    (module Array_value : VALUE);
    (module Lazy_value : VALUE);
    (module Lazy_fourced_value : VALUE);
    (module Function_value : VALUE);
    (module Record_value : VALUE);
  ]

(* Orders sensitive *)

let modules_tbl =
  Hashtbl.of_seq
    ( modules |> List.to_seq
    |> Seq.map (fun (module Value : VALUE) ->
           (Value.extension_constructor, (module Value : VALUE))) )

let find_module v =
  try
    let ec = Obj.Extension_constructor.of_val v in
    Hashtbl.find modules_tbl ec
  with Not_found -> (module Unknown_value : VALUE)

let adopt conn env ty rv =
  let rec resolve_type ty =
    match (Ctype.repr ty).desc with
    | Tlink ty | Tsubst ty | Tpoly (ty, _) -> resolve_type ty
    | Tconstr (path, ty_args, _) -> (
        match Env.find_type path env with
        | exception Not_found -> ty
        | {
         type_kind = Type_abstract;
         type_manifest = Some body;
         type_params;
         _;
        } -> (
            match Ctype.apply env type_params body ty_args with
            | ty -> resolve_type ty
            | exception Ctype.Cannot_apply -> ty )
        | _ -> ty )
    | _ -> ty
  in
  let ty = resolve_type ty in
  try%lwt
    modules |> List.to_seq
    |> Lwt_util.find_map_seq_s (fun (module Value : VALUE) ->
           Value.adopt conn env ty rv)
  with Not_found -> Lwt.return Unknown_value.Unknown

let to_short_string ?(hex = false) v =
  let (module Value : VALUE) = find_module v in
  Value.to_short_string ~hex v

let is_named_container v =
  let (module Value : VALUE) = find_module v in
  Value.is_named_container

let is_indexed_container v =
  let (module Value : VALUE) = find_module v in
  Value.is_indexed_container

let get_indexed v index =
  let (module Value : VALUE) = find_module v in
  Value.get_indexed v index

let num_indexed v =
  let (module Value : VALUE) = find_module v in
  Value.num_indexed v

let num_named v =
  let (module Value : VALUE) = find_module v in
  Value.num_named v

let list_named v =
  let (module Value : VALUE) = find_module v in
  Value.list_named v

let () = rec_adopt := adopt