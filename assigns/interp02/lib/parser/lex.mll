{
  open Par
}

let whitespace = [' ' '\t' '\n' '\r']+

let digit = ['0'-'9']

let num = '-'? digit+

let var = ['a'-'z' '_' ] [ 'a'-'z' 'A'-'Z' '0'-'9' '_' '\'' ]*

rule read = parse
  | whitespace { read lexbuf }
  | num { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
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
      | "unit" -> UNIT
      | _ -> IDENT id
    }
  | "->" { ARROW }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | ":" { COLON }
  | "=" { EQUAL }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "mod" { MOD }
  | "<=" { LTE }
  | ">=" { GTE }
  | "<>" { NEQ }
  | "<" { LT }
  | ">" { GT }
  | "&&" { AND }
  | "||" { OR }
  | "()" { UNIT }
  | eof { EOF }
  |_ as ch { failwith ("Unexpected char: " ^ String.make 1 ch) }
