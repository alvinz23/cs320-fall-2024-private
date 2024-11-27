

let parse s =
  try 
    Some (Par.prog Lex.read (Lexing.from_string s))
  with e ->
    Printf.printf "Parse error: %s\n" (Printexc.to_string e);
    None 
