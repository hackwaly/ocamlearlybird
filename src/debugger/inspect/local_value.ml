open Value

class virtual ['a] local_value (v : 'a) =
  object
    inherit abstract_value

    method value = v

    method num_indexed _conn = Lwt.return 0

    method get_indexed = assert false

    method num_named _conn = Lwt.return 0

    method get_name = assert false

    method get_named = assert false
  end

class int_value v =
  object
    inherit [int] local_value v as super

    method to_short_string = Int.to_string super#value
  end

class float_value v =
  object
    inherit [float] local_value v as super

    method to_short_string = Float.to_string super#value
  end

class bool_value v =
  object
    inherit [bool] local_value v as super

    method to_short_string = Bool.to_string super#value
  end

class char_value v =
  object
    inherit [char] local_value v as super

    method to_short_string = Char.escaped super#value
  end

class string_value v =
  object
    inherit [string] local_value v as super

    method to_short_string = String.escaped super#value
  end
