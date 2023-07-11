{
open Trivia_parser
}

rule comment = parse
  | "*)" {}
  | "(*" { comment lexbuf; comment lexbuf }
  | _+ { comment lexbuf }

and token = parse
  | eof { EOF }
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | ";" { SEMICOLON }
  | "(*)" { token lexbuf }
  | "(*" { comment lexbuf; token lexbuf }
  | "in" { IN }
  | _ { failwith "" }
