let check str =
  try
    let lexbuf = Lexing.from_string str in
    Trivia_parser.check Trivia_lexer.token lexbuf;
    true
  with _ ->
    false
