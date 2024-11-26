{
  open Par
}

let whitespace = [' ' '\n' '\t' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
    | num  { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    | var { VAR (Lexing.lexeme lexbuf) }
    | whitespace { read lexbuf }
    | eof { EOF }
    | "int"               { INT }
    | "bool"              { BOOL }
    | "true"              { TRUE }
    | "false"             { FALSE }
    | "unit"              { UNIT }
    | "if"                { IF }
    | "then"              { THEN }
    | "else"              { ELSE }
    | "let"               { LET }
    | "rec"               { REC }
    | "in"                { IN }
    | "fun"               { FUN }
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
    | "unit"              { UNIT }
    | "assert"            { ASSERT }
    | "->"                { ARROW }
    | "="                 { EQUALS }
    | '('                 { LPAREN }
    | ')'                 { RPAREN }
    | ':'                 { COLON }