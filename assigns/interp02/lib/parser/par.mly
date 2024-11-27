%{
  open Utils
  let rec mk_app e es =
    match es with
    | [] -> e
    | h :: t -> mk_app (SApp (e, h)) t
%}

%token <string> VAR
%token <int> NUM
%token LET
%token EQUALS
%token IN
%token REC
%token COLON
%token LPAREN
%token RPAREN
%token INT
%token BOOL
%token UNIT
%token ARROW
%token IF
%token THEN
%token ELSE
%token ASSERT
%token TRUE
%token FALSE
%token FUN
%token MOD
%token AND
%token OR
%token LT
%token LTE
%token GT
%token GTE
%token NEQ
%token ADD
%token SUB
%token MUL
%token DIV
%token EOF

%right OR
%right AND
%left LT LTE GT GTE NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog

%%

prog:
  | top_lets EOF { $1 }
;

top_lets:
  | toplet top_lets { $1 :: $2 }
  | { [] }
;

toplet:
  | LET VAR args COLON ty EQUALS expr
    { { is_rec = false; name = $2; args = $3; ty = $5; value = $7 } }
  | LET REC VAR arg args COLON ty EQUALS expr
    { { is_rec = true; name = $3; args = $4 :: $5; ty = $7; value = $9 } }
;

args:
  | arg args { $1 :: $2 }
  |  { [] }
;

arg:
  | LPAREN VAR COLON ty RPAREN { ($2, $4) }
;

ty:
  | INT { IntTy }
  | BOOL { BoolTy }
  | UNIT { UnitTy }
  | ty ARROW ty { FunTy($1, $3) }
  | LPAREN ty RPAREN { $2 }
;

expr:
  | LET VAR args COLON ty EQUALS expr IN expr
    { SLet { is_rec = false; name = $2; args = $3; ty = $5; value = $7; body = $9 } }
  | LET REC VAR arg args COLON ty EQUALS expr IN expr
    { SLet { is_rec = true; name = $3; args = $4 :: $5; ty = $7; value = $9; body = $11 } }
  | IF expr THEN expr ELSE expr %prec THEN
    { SIf($2, $4, $6) }
| FUN first_arg = arg rest_args = list(arg) ARROW body = expr
    { SFun {arg = first_arg; args = rest_args; body} }
  | expr2 { $1 }
;

expr2:
  | e1 = expr2 op = bop e2 = expr2 
    { SBop(op, e1, e2) }
  | ASSERT e = expr3 
    { SAssert(e) }
  | func = expr3 args = nonempty_list(expr3) 
    { mk_app func args }
  | e = expr3 { e }
;

expr3:
  | expr3 AND expr4 { SBop(And, $1, $3) }
  | expr4 { $1 }
;

expr4:
  | expr4 ADD expr5 { SBop(Add, $1, $3) }
  | expr4 SUB expr5 { SBop(Sub, $1, $3) }
  | expr5 { $1 }
;

expr5:
  | expr5 MUL expr6 { SBop(Mul, $1, $3) }
  | expr5 DIV expr6 { SBop(Div, $1, $3) }
  | expr6 { $1 }
;

expr6:
  | LPAREN expr RPAREN { $2 }
  | NUM { SNum($1) }
  | VAR { SVar($1) }
  | LPAREN RPAREN { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
;

%inline bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }
;