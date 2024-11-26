%{
  open Utils
  let rec apply func args =
    match args with
    | [] -> func
    | head :: tail -> apply (SApp (func, head)) tail
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
%left LT LTE GT GTE EQUALS NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog

%%

prog:
  declarations EOF { $1 }
;

declarations:
  { [] }
| declarations binding { $1 @ [$2] }
;

binding:
  LET name=VAR params=optional_parameters COLON ty=type_expression EQUALS expr_body=expression {
    { is_rec = false; name; args = params; ty; value = expr_body }
  }
| LET REC name=VAR first_param=parameter additional_params=more_parameters COLON ty=type_expression EQUALS expr_body=expression {
    { is_rec = true; name; args = first_param :: additional_params; ty; value = expr_body }
  }
;

optional_parameters:
  { [] }
| parameter_list { $1 }
;

parameter_list:
  parameter { [$1] }
| parameter_list parameter { $1 @ [$2] }
;

parameter:
  LPAREN param_name=VAR COLON param_type=type_expression RPAREN { (param_name, param_type) }
;

more_parameters:
  { [] }
| more_parameters parameter { $1 @ [$2] }
;

type_expression:
  INT { IntTy }
| BOOL { BoolTy }
| UNIT { UnitTy }
| from_ty=type_expression ARROW to_ty=type_expression { FunTy (from_ty, to_ty) }
| LPAREN inner_ty=type_expression RPAREN { inner_ty }
;

expression:
  LET name=VAR params=optional_parameters COLON ty=type_expression EQUALS value=expression IN body=expression {
    SLet { is_rec = false; name; args = params; ty; value = value; body = body }
  }
| LET REC name=VAR first_param=parameter additional_params=more_parameters COLON ty=type_expression EQUALS value=expression IN body=expression {
    SLet { is_rec = true; name; args = first_param :: additional_params; ty; value = value; body = body }
  }
| IF condition=expression THEN then_branch=expression ELSE else_branch=expression {
    SIf (condition, then_branch, else_branch)
  }
| FUN first_param=parameter additional_params=more_parameters ARROW func_body=expression {
    SFun { arg = first_param; args = additional_params; body = func_body }
  }
| expr_lvl1 { $1 }
;

expr_lvl1:
  expr_lvl1 operator=binary_operator expr_lvl1 { SBop (operator, $1, $3) }
| ASSERT expr_lvl2 { SAssert ($2) }
| func=expr_lvl2 args=expr_arguments { apply func args }
| expr_lvl2 { $1 }
;

expr_arguments:
  expr_lvl2 { [ $1 ] }
| expr_arguments expr_lvl2 { $1 @ [ $2 ] }
;

expr_lvl2:
  LPAREN RPAREN { SUnit }
| TRUE { STrue }
| FALSE { SFalse }
| n=NUM { SNum n }
| x=VAR { SVar x }
| LPAREN nested_expr=expression RPAREN { nested_expr }
;

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
| EQUALS { Eq }
| NEQ { Neq }
| AND { And }
| OR { Or }
;
