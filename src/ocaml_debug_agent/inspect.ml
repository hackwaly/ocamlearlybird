open Inspect_types

let get_value env rv ty marshal =
  if Ctype.matches env Predef.type_int ty then (
    let%lwt mv = marshal rv in
    Lwt.return (Int (Obj.magic mv))
  ) else if (Ctype.matches env Predef.type_float ty) then (
    let%lwt mv = marshal rv in
    Lwt.return (Double (Obj.magic mv))
  ) else if (Ctype.matches env Predef.type_bool ty) then (
    let%lwt mv = marshal rv in
    Lwt.return (Bool (Obj.magic mv))
  ) else if (Ctype.matches env Predef.type_char ty) then (
    let%lwt mv = marshal rv in
    Lwt.return (Char (Obj.magic mv))
  ) else if (Ctype.matches env Predef.type_string ty) then (
    let%lwt mv = marshal rv in
    Lwt.return (String (Obj.magic mv))
  ) else (
    Lwt.return Unknown
  )
