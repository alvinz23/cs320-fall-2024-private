{
  open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
  | "true"              { TRUE }
  | "false"             { FALSE }
  | "unit"              { UNIT }
  | "int"               { INT }
  | "bool"              { BOOL }
  | "if"                { IF }
  | "then"              { THEN }
  | "else"              { ELSE }
  | "let"               { LET }
  | "rec"               { REC }
  | "in"                { IN }
  | "fun"               { FUN }
  | "assert"            { ASSERT }
  | "->"                { ARROW }
  | "="                 { EQUALS }
  | "<>"                { NEQ }
  | "<="                { LTE }
  | "<"                 { LT }
  | ">="                { GTE }
  | ">"                 { GT }
  | "+"                 { ADD }
  | "-"                 { SUB }
  | "*"                 { MUL }
  | "/"                 { DIV }
  | "mod"               { MOD }
  | "&&"                { AND }
  | "||"                { OR }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | ':'                 { COLON }
  | num                 { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var as id           { VAR id }
  | whitespace          { read lexbuf }
  | eof                 { EOF }