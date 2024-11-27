%{
  open Utils
  let rec apply_args func args =
    match args with
    | [] -> func
    | x :: xs -> apply_args (SApp (func, x)) xs
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
  | definitions = list(definition) EOF { definitions }
;

definition:
  | LET id = VAR params = list(param) COLON typ = type_expr EQUALS body = expression
    { { is_rec = false; name = id; args = params; ty = typ; value = body } }
  | LET REC id = VAR first_param = param other_params = list(param) COLON typ = type_expr 
    EQUALS body = expression
    { { is_rec = true; name = id; args = first_param :: other_params; ty = typ; value = body } }
;

param:
  | LPAREN id = VAR COLON typ = type_expr RPAREN 
    { (id, typ) }
;

type_expr:
  | INT { IntTy }
  | BOOL { BoolTy }
  | UNIT { UnitTy }
  | t1 = type_expr ARROW t2 = type_expr { FunTy(t1, t2) }
  | LPAREN inner_type = type_expr RPAREN { inner_type }
;

expression:
  | LET id = VAR params = list(param) COLON typ = type_expr EQUALS value = expression IN body = expression
    { SLet { is_rec = false; name = id; args = params; ty = typ; value; body } }
  | LET REC id = VAR first_param = param other_params = list(param) COLON typ = type_expr 
    EQUALS value = expression IN body = expression
    { SLet { is_rec = true; name = id; args = first_param :: other_params; ty = typ; value; body } }
  | IF cond = expression THEN if_body = expression ELSE else_body = expression
    { SIf(cond, if_body, else_body) }
  | FUN first_param = param other_params = list(param) ARROW body = expression
    { SFun { arg = first_param; args = other_params; body } }
  | term_expr = term { term_expr }
;

term:
  | left_term = term bin_op = operator right_term = term 
    { SBop(bin_op, left_term, right_term) }
  | ASSERT cond = primary_expr 
    { SAssert(cond) }
  | func = primary_expr args = nonempty_list(primary_expr) 
    { apply_args func args }
  | primary_expr = primary_expr { primary_expr }
;

primary_expr:
  | LPAREN RPAREN { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | num_val = NUM { SNum num_val }
  | var_name = VAR { SVar var_name }
  | LPAREN expr = expression RPAREN { expr }
;

%inline operator:
  | ADD { Add }
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
