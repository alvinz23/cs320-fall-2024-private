

%{
  open Utils
%}


%token IF THEN ELSE
%token LET EQUAL IN
%token FUN ARROW
%token AND OR
%token PLUS MINUS TIMES DIVIDE MOD
%token LT LEQ GT GEQ EQ NEQ
%token LPAREN RPAREN
%token TRUE FALSE UNIT
%token <int> NUM
%token <string> VAR
%token EOF

%start <prog> prog

%right OR
%right AND
%left LT LEQ GT GEQ EQ NEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD

%%

prog:
  | expr EOF { Some $1 }

expr:
  | IF expr THEN expr ELSE expr { If ($2, $4, $6) }
  | LET VAR EQUAL expr IN expr { Let ($2, $4, $6) }
  | FUN VAR ARROW expr { Fun ($2, $4) }
  | expr2 { $1 }

expr2:
  | expr2 bop expr2 { Bop ($2, $1, $3) }
  | expr_app { $1 }

expr_app:
  | expr_app expr3 { App ($1, $2) }
  | expr3 { $1 }

expr3:
  | UNIT { Unit }
  | TRUE { True }
  | FALSE { False }
  | NUM { Num $1 }
  | VAR { Var $1 }
  | LPAREN expr RPAREN { $2 }

bop:
  | PLUS { Add }
  | MINUS { Sub }
  | TIMES { Mul }
  | DIVIDE { Div }
  | MOD { Mod }
  | LT { Lt }
  | LEQ { Lte }
  | GT { Gt }
  | GEQ { Gte }
  | EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }
