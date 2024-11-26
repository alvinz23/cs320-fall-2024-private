


let parse s =
  try Some (Par.program Lex.read (Lexing.from_string s))
  with _ -> None
