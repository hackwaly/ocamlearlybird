open Stdlib
include Lexing

module Position = struct
  type t = position

  let line t = t.pos_lnum

  let column t = t.pos_cnum - t.pos_bol + 1

  let line_column t = (line t, column t)
end
