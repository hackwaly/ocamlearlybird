type pc = int * int

[%%if ocaml_version < (5, 0, 0)]

module Sp = struct

  type t = int

  let null = 0
  let mone = -1

  let base sp n = sp - n

  let compare sp1 sp2 = Stdlib.compare sp1 sp2


  let read in_ =
    let%lwt sp = Lwt_io.BE.read_int in_ in
    Lwt.return sp

  let write out sp =
    Lwt_io.BE.write_int out sp
end

let main_frag = 0

[%%else]

module Sp = struct

  (* Position in the debuggee's stack. *)
  type t = {
    block : int;
    offset : int;
  }

  let null = { block = -1; offset = -1}
  let mone = { block = -1; offset = -1}

  let base sp n = {sp with offset = sp.offset - n}

  let compare sp1 sp2 =
    match Stdlib.compare sp1.block sp2.block with
    | 0 -> Stdlib.compare sp1.offset sp2.offset
    | x -> x


  let read in_ =
    let%lwt block = Lwt_io.BE.read_int in_ in
    let%lwt offset = Lwt_io.BE.read_int in_ in
    Lwt.return {block; offset}

  let write out {block; offset} =
    Lwt_io.BE.write_int out block;%lwt
    Lwt_io.BE.write_int out offset
end

(* Identifier of the code fragment for the main program.
   Numbering starts at 1 and the runtime registers 2 fragments before
   the main program: one for uncaught exceptions and one for callbacks.
*)
let main_frag = 3

[%%endif]

type 'a source_location = {
  source : string;
  pos : int * int;
  end_ : 'a;
}

type source_position = unit source_location

type source_range = (int * int) source_location
