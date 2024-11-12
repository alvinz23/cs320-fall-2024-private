

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
%token APP
%token <int> NUM

%token <string> VAR

%token EOF





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


