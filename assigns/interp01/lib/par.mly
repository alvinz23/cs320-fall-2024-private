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

assigns/assign07/lib/parser/my_parser.ml
Download



let parse s =

  try Some (Par.prog Lex.read (Lexing.from_string s))

  with _ -> None


assigns/assign07/lib/parser/par.mly
Download





%{

open Utils

%}


%token IF THEN ELSE LET IN FUN MOD TRUE FALSE

%token ARROW

%token LPAREN RPAREN

%token UNIT

%token AND OR

%token LT LTE GT GTE EQ NEQ

%token PLUS MINUS TIMES DIVIDE

%token <int> NUM

%token <string> VAR

%token EOF


%right OR

%right AND

%left LT LTE GT GTE EQ NEQ

%left PLUS MINUS

%left TIMES DIVIDE MOD

%left APP


%start <Utils.prog> prog


%%


prog:

  expr EOF { $1 }


expr:

  | IF expr THEN expr ELSE expr { If ($2, $4, $6) }

  | LET VAR EQ expr IN expr { Let ($2, $4, $6) }

  | FUN VAR ARROW expr { Fun ($2, $4) }

  | expr1 { $1 }


expr1:

  | expr1 OR expr2 { Bop (Or, $1, $3) }

  | expr2 { $1 }


expr2:

  | expr2 AND expr3 { Bop (And, $1, $3) }

  | expr3 { $1 }


expr3:

  | expr3 relop expr4 { Bop ($2, $1, $3) }

  | expr4 { $1 }


expr4:

  | expr4 PLUS expr5 { Bop (Add, $1, $3) }

  | expr4 MINUS expr5 { Bop (Sub, $1, $3) }

  | expr5 { $1 }


expr5:

  | expr5 TIMES expr6 { Bop (Mul, $1, $3) }

  | expr5 DIVIDE expr6 { Bop (Div, $1, $3) }

  | expr5 MOD expr6 { Bop (Mod, $1, $3) }

  | expr6 { $1 }


expr6:

  | expr6 expr7 %prec APP { App ($1, $2) }

  | expr7 { $1 }


expr7:

  | LPAREN expr RPAREN { $2 }

  | UNIT { Unit }

  | TRUE { True }

  | FALSE { False }

  | NUM { Num $1 }

  | VAR { Var $1 }


relop:

  | LT { Lt }

  | LTE { Lte }

  | GT { Gt }

  | GTE { Gte }

  | EQ { Eq }

  | NEQ { Neq }


