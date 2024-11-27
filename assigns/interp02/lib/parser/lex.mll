{

open Par

}

let whitespace = [' ' '\t' '\n' '\r']+

let digit = ['0'-'9']

let number = '-'? digit+

let identifier = [ 'a'-'z' '_' '$' ] [ 'a'-'z' 'A'-'Z' '0'-'9' '_' '\'' ]*

rule read = parse
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
    | "int"               { INT }
    | "bool"              { BOOL }
    | number              { NUM (int_of_string (Lexing.lexeme lexbuf)) }
    | identifier          { VAR (Lexing.lexeme lexbuf) }
    | eof                 { EOF }
