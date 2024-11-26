%{
  open Utils
%}

%token LET REC IN IF THEN ELSE FUN ASSERT TRUE FALSE
%token INT BOOL UNIT_TYPE
%token LPAREN RPAREN COLON EQUAL
%token DIVIDE
%token AND OR
%token UNIT
%token <int> NUM
%token <string> VAR
%token EOF
%token PLUS MINUS TIMES MOD
%token LT LTE GT GTE EQ NEQ
%token ARROW

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
  | expr OR expr { SBop (Or, $1, $3) }
  | expr AND expr { SBop (And, $1, $3) }
  | expr EQ expr { SBop (Eq, $1, $3) }
  | expr NEQ expr { SBop (Neq, $1, $3) }
  | expr LT expr { SBop (Lt, $1, $3) }
  | expr LTE expr { SBop (Lte, $1, $3) }
  | expr GT expr { SBop (Gt, $1, $3) }
  | expr GTE expr { SBop (Gte, $1, $3) }
  | expr PLUS expr { SBop (Add, $1, $3) }
  | expr MINUS expr { SBop (Sub, $1, $3) }
  | expr TIMES expr { SBop (Mul, $1, $3) }
  | expr DIVIDE expr { SBop (Div, $1, $3) }
  | expr MOD expr { SBop (Mod, $1, $3) }
  | expr expr %prec APPLY { SApp ($1, $2) }
  | ASSERT expr { SAssert $2 }
  | LPAREN expr RPAREN { $2 }
  | UNIT { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | NUM { SNum $1 }
  | VAR { SVar $1 }

