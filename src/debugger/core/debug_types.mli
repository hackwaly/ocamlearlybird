type pc = int * int

module Sp : sig
  type t
  val null : t
  val mone : t
  val base : t -> int -> t
  val compare : t -> t -> int

  val read : Lwt_io.input_channel -> t Lwt.t
  val write : Lwt_io.output_channel -> t -> unit Lwt.t
end

val main_frag : int

type 'a source_location = {
  source : string;
  pos : int * int;
  end_ : 'a;
}

type source_position = unit source_location

type source_range = (int * int) source_location
