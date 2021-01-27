open Stdlib
include List

(* CREDIT: Copied from batteries BatList.mlv *)
type 'a mut_list = { hd : 'a; mutable tl : 'a list }

external inj : 'a mut_list -> 'a list = "%identity"

module Acc = struct
  let dummy () = { hd = Obj.magic (); tl = [] }

  let create x = { hd = x; tl = [] }

  let accum acc x =
    let cell = create x in
    acc.tl <- inj cell;
    cell
end

let span p li =
  let rec loop dst = function
    | [] -> []
    | x :: xs as l -> if p x then loop (Acc.accum dst x) xs else l
  in
  let dummy = Acc.dummy () in
  let xs = loop dummy li in
  (dummy.tl, xs)

let group_consecutive p l =
  let rec loop dst = function
    | [] -> ()
    | x :: rest ->
        let xs, rest = span (p x) rest in
        loop (Acc.accum dst (x :: xs)) rest
  in
  let dummy = Acc.dummy () in
  loop dummy l;
  dummy.tl

let uniq_cons x xs = if List.mem x xs then xs else x :: xs

let uniq xs = List.fold_right uniq_cons xs []
