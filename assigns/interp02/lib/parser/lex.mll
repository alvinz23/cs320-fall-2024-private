{
  open Par
}

let whitespace = [' ' '\t' '\n' '\r']+

let digit = ['0'-'9']

let num = '-'? digit+

let var = ['a'-'z' '_' ] [ 'a'-'z' 'A'-'Z' '0'-'9' '_' '\'' ]*

rule read = parse
  | whitespace { read lexbuf }
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var as id {
      match id with
      | "let" -> LET
      | "rec" -> REC
      | "in" -> IN
      | "if" -> IF
      | "then" -> THEN
      | "else" -> ELSE
      | "fun" -> FUN
      | "assert" -> ASSERT
      | "mod" -> MOD
      | "true" -> TRUE
      | "false" -> FALSE
      | "int" -> INT
      | "bool" -> BOOL
      | "unit" -> UNIT_TYPE
      | _ -> VAR id
    }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ":" { COLON }
  | "=" { EQUAL }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDE }
  | "<=" { LTE }
  | ">=" { GTE }
  | "<>" { NEQ }
  | "<" { LT }
  | ">" { GT }
  | "&&" { AND }
  | "||" { OR }
  | "()" { UNIT }
  | eof { EOF }
  |_ as ch { failwith ("Unexpected character: " ^ String.make 1 ch) }

