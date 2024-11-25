%{
  open Utils
%}


%token LET REC IN IF THEN ELSE FUN ASSERT MOD TRUE FALSE
%token INT BOOL UNIT_TYPE
%token LPAREN RPAREN COLON EQUAL ARROW
%token PLUS MINUS TIMES DIVIDE
%token LT LTE GT GTE EQ NEQ
%token AND OR
%token UNIT
%token <int> NUM
%token <string> VAR
%token EOF


%right ARROW
%left OR
%left AND
%nonassoc EQ NEQ LT LTE GT GTE
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left APPLY  


%start <Utils.prog> prog


%%

prog:
  | toplet_list EOF { List.rev $1 }

toplet_list:
  | /* empty */ { [] }
  | toplet_list toplet { $2 :: $1 }

toplet:
  | LET VAR arg_list_opt COLON ty EQUAL expr {
      { is_rec = false; name = $2; args = $3; ty = $5; value = $7 }
    }
  | LET REC VAR arg_list arg_list_opt COLON ty EQUAL expr {
      { is_rec = true; name = $3; args = $4 @ $5; ty = $7; value = $9 }
    }

arg_list_opt:
  | arg_list { $1 }
  | /* empty */ { [] }

arg_list:
  | arg_list arg { $1 @ [$2] }
  | arg { [$1] }

arg:
  | LPAREN VAR COLON ty RPAREN { ($2, $4) }

ty:
  | INT { IntTy }
  | BOOL { BoolTy }
  | UNIT_TYPE { UnitTy }
  | ty ARROW ty { FunTy ($1, $3) }
  | LPAREN ty RPAREN { $2 }

expr:
  | LET VAR arg_list_opt COLON ty EQUAL expr IN expr {
      SLet { is_rec = false; name = $2; args = $3; ty = $5; value = $7; body = $9 }
    }
  | LET REC VAR arg_list arg_list_opt COLON ty EQUAL expr IN expr {
      SLet { is_rec = true; name = $3; args = $4 @ $5; ty = $7; value = $9; body = $11 }
    }
  | IF expr THEN expr ELSE expr {
      SIf ($2, $4, $6)
    }
  | FUN arg_list ARROW expr {
      match $2 with
      | arg :: args -> SFun { arg; args; body = $4 }
      | [] -> failwith "Function must have at least one argument"
    }
  | expr1 { $1 }

expr1:
  | expr1 OR expr2 { SBop (Or, $1, $3) }
  | expr2 { $1 }

expr2:
  | expr2 AND expr3 { SBop (And, $1, $3) }
  | expr3 { $1 }

expr3:
  | expr3 relop expr4 { SBop ($2, $1, $3) }
  | expr4 { $1 }

expr4:
  | expr4 PLUS expr5 { SBop (Add, $1, $3) }
  | expr4 MINUS expr5 { SBop (Sub, $1, $3) }
  | expr5 { $1 }

expr5:
  | expr5 TIMES expr6 { SBop (Mul, $1, $3) }
  | expr5 DIVIDE expr6 { SBop (Div, $1, $3) }
  | expr5 MOD expr6 { SBop (Mod, $1, $3) }
  | expr6 { $1 }

expr6:
  | expr6 expr7 %prec APPLY { SApp ($1, $2) }
  | expr7 { $1 }

expr7:
  | LPAREN expr RPAREN { $2 }
  | UNIT { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | NUM { SNum $1 }
  | VAR { SVar $1 }
  | ASSERT expr7 { SAssert $2 }

relop:
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ { Neq }

