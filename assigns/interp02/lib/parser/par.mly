%{
  open Utils
  let rec build_application func args =
    match args with
    | [] -> func
    | arg :: rest -> build_application (SApp (func, arg)) rest
%}

%token <string> IDENT
%token <int> NUMBER
%token LET REC IN IF THEN ELSE FUN ASSERT TRUE FALSE
%token INT BOOL UNIT
%token LPAREN RPAREN COLON EQUAL
%token ARROW
%token ADD SUB MUL DIV MOD
%token AND OR
%token LT LTE GT GTE EQ NEQ
%token EOF

%right OR
%right AND
%nonassoc LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> program

%%

program:
    toplet_list EOF { $1 }
;

toplet_list:
 { [] }
  | toplet_list toplet { $1 @ [$2] }
;

toplet:
    LET name=IDENT params=param_list_opt COLON typ=type_expr EQUAL expr=expression {
      { is_rec = false; name; args = params; ty = typ; value = expr }
    }
  | LET REC name=IDENT first_param=param rest_params=param_list COLON typ=type_expr EQUAL expr=expression {
      { is_rec = true; name; args = first_param :: rest_params; ty = typ; value = expr }
    }
;

param_list_opt:
    { [] }
  | param_list { $1 }
;

param_list:
    param { [$1] }
  | param_list param { $1 @ [$2] }
;

param:
    LPAREN name=IDENT COLON typ=type_expr RPAREN { (name, typ) }
;

type_expr:
    INT { IntTy }
  | BOOL { BoolTy }
  | UNIT { UnitTy }
  | LPAREN type_expr RPAREN { $2 }
  | t1=type_expr ARROW t2=type_expr { FunTy (t1, t2) }
;

expression:
    LET name=IDENT params=param_list_opt COLON typ=type_expr EQUAL value=expression IN body=expression {
      SLet { is_rec = false; name; args = params; ty = typ; value; body }
    }
  | LET REC name=IDENT first_param=param rest_params=param_list COLON typ=type_expr EQUAL value=expression IN body=expression {
      SLet { is_rec = true; name; args = first_param :: rest_params; ty = typ; value; body }
    }
  | IF condition=expression THEN true_branch=expression ELSE false_branch=expression {
      SIf (condition, true_branch, false_branch)
    }
  | FUN first_param=param rest_params=param_list ARROW body=expression {
      SFun { arg = first_param; args = rest_params; body }
    }
  | expr=expression_level1 { expr }
;

expression_level1:
    expression_level1 op=binary_operator expression_level1 {
      SBop (op, $1, $3)
    }
  | ASSERT expr=expression_level2 {
      SAssert expr
    }
  | func=expression_level2 args=nonempty_expr_list {
      build_application func args
    }
  | expr=expression_level2 {
      expr
    }
;

(* Non-empty list of expressions *)
nonempty_expr_list:
    expr=expression_level2 { [$1] }
  | nonempty_expr_list expr=expression_level2 { $1 @ [$2] }
;

(* Atomic expressions *)
expression_level2:
    LPAREN RPAREN { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | NUMBER { SNum $1 }
  | IDENT { SVar $1 }
  | LPAREN expr=expression RPAREN { expr }
;

(* Binary operators *)
%inline binary_operator:
    ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT { Lt }
  | LTE { Lte }
  | GT { Gt }
  | GTE { Gte }
  | EQ { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }
;
