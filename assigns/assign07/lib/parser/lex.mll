

{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let num = '-'? digit+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | whitespace { read lexbuf }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var as id {
      match id with
      | "if" -> IF
      | "then" -> THEN
      | "else" -> ELSE
      | "let" -> LET
      | "in" -> IN
      | "fun" -> FUN
      | "mod" -> MOD
      | "true" -> TRUE
      | "false" -> FALSE
      | _ -> VAR id
    }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "&&" { AND }
  | "||" { OR }
  | "<=" { LTE }
  | ">=" { GTE }
  | "<>" { NEQ }
  | "<" { LT }
  | ">" { GT }
  | "=" { EQ }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDE }
  | "()" { UNIT }
  | eof { EOF }