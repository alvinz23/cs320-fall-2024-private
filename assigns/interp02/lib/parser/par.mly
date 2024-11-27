

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
%left LT LTE GT GTE EQUALS NEQ
%left ADD SUB
%left MUL DIV MOD

%start <Utils.prog> prog

%%

prog:
  toplet_list = list_toplet EOF { toplet_list }
;

toplet:
  | LET name = VAR args = list_arg COLON ty = ty EQUALS value = expr
    { {is_rec = false; name; args; ty; value} }
  | LET REC name = VAR first_arg = arg rest_args = list_arg COLON ty = ty EQUALS value = expr
    { {is_rec = true; name; args = first_arg :: rest_args; ty; value} }
;

list_toplet:
  { [] }
| list_toplet toplet { $1 @ [$2] }
;

arg:
  | LPAREN name = VAR COLON ty_decl = ty RPAREN 
    { (name, ty_decl) }
;

ty:
  | INT { IntTy }
  | BOOL { BoolTy }
  | UNIT { UnitTy }
  | t1 = ty ARROW t2 = ty { FunTy(t1, t2) }
  | LPAREN t = ty RPAREN { t }
;

expr:
  | LET name = VAR args = list_arg COLON ty_decl = ty EQUALS value = expr IN body = expr
    { SLet {is_rec = false; name; args = args; ty = ty_decl; value; body} }
  | LET REC name = VAR first_arg = arg rest_args = list_arg COLON ty_decl = ty 
    EQUALS value = expr IN body = expr
    { SLet {is_rec = true; name; args = first_arg :: rest_args; ty = ty_decl; value; body} }
  | IF cond = expr THEN then_expr = expr ELSE else_expr = expr
    { SIf(cond, then_expr, else_expr) }
  | FUN first_arg = arg rest_args = list_arg ARROW body = expr
    { SFun {arg = first_arg; args = rest_args; body} }
  | e = expr2 { e }
;

list_arg:
  { [] }
| list_arg arg { $1 @ [$2] }
;

expr2:
  | e1 = expr2 op = bop e2 = expr2 
    { SBop(op, e1, e2) }
  | ASSERT e = expr3 
    { SAssert(e) }
  | func = expr3 args = nonempty_list_expr3 
    { mk_app func args }
  | e = expr3 { e }
;

nonempty_list_expr3:
  expr3 { [$1] }
| nonempty_list_expr3 expr3 { $1 @ [$2] }
;

expr3:
  | LPAREN RPAREN { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | n = NUM { SNum n }
  | x = VAR { SVar x }
  | LPAREN e = expr RPAREN { e }
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
  | EQUALS { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR { Or }
; 
