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
  | toplet_list = list(toplet) EOF { toplet_list }
;

toplet:
  | LET name = VAR args = list(arg) COLON ty = ty EQUALS value = expression1
    { {is_rec = false; name; args; ty; value} }
  | LET REC name = VAR first_arg = arg rest_args = list(arg) COLON ty = ty EQUALS value = expression1
    { {is_rec = true; name; args = first_arg :: rest_args; ty; value} }
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

expression1:
  | LET name = VAR args = list(arg) COLON ty_decl = ty EQUALS value = expression1 IN body = expression1
    { SLet {is_rec = false; name; args; ty = ty_decl; value; body} }
  | LET REC name = VAR first_arg = arg rest_args = list(arg) COLON ty_decl = ty 
    EQUALS value = expression1 IN body = expression1
    { SLet {is_rec = true; name; args = first_arg :: rest_args; ty = ty_decl; value; body} }
  | IF cond = expression1 THEN then_expression1 = expression1 ELSE else_expression1 = expression1
    { SIf(cond, then_expression1, else_expression1) }
  | FUN first_arg = arg rest_args = list(arg) ARROW body = expression1
    { SFun {arg = first_arg; args = rest_args; body} }
  | e = expression2 { e }
;

expression2:
  | e1 = expression2 op = bop e2 = expression2 
    { SBop(op, e1, e2) }
  | ASSERT e = expression3 
    { SAssert(e) }
  | func = expression3 args = nonempty_list(expression3) 
    { mk_app func args }
  | e = expression3 { e }
;

expression3:
  | LPAREN RPAREN { SUnit }
  | TRUE { STrue }
  | FALSE { SFalse }
  | n = NUM { SNum n }
  | x = VAR { SVar x }
  | LPAREN e = expression1 RPAREN { e }
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