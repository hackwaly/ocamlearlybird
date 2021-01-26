open Stdlib
include Option

let ( let* ) o f = match o with None -> None | Some x -> f x

let return x = Some x
