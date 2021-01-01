class virtual abstract_value =
  object
    method virtual to_short_string : string

    method virtual num_indexed : Debugcom.conn -> int Lwt.t

    method virtual get_indexed : Debugcom.conn -> int -> abstract_value Lwt.t

    method virtual num_named : Debugcom.conn -> int Lwt.t

    method virtual get_name : Debugcom.conn -> int -> string Lwt.t

    method virtual get_named : Debugcom.conn -> int -> abstract_value Lwt.t
  end
