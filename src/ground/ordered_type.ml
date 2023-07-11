module type S = sig
  type t

  val compare : t -> t -> int
end

module Make_tuple2 (T1 : S) (T2 : S) : S with type t = T1.t * T2.t = struct
  type t = T1.t * T2.t

  let compare = Compare.chain [ Compare.by fst; Compare.by snd ]
end
