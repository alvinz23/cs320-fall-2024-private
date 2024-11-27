
open Utils
open Stdlib320

type ty_env = ty Stdlib320.env
type val_env = value Stdlib320.env
module Env = Stdlib320.Env


exception AssertFail
exception DivByZero



let parse : string -> prog option = 
  fun s -> My_parser.parse s 
  
let (>>=) r f =
  match r with
  | Ok v -> f v
  | Error e -> Error e

let expected_types_of_bop op =
  match op with
  | Add | Sub | Mul | Div | Mod ->
      (IntTy, IntTy, IntTy)
  | Lt | Lte | Gt | Gte | Eq | Neq ->
      (IntTy, IntTy, BoolTy)
  | And | Or ->
      (BoolTy, BoolTy, BoolTy)

let rec desugar prog =
  let rec desugar_toplets toplets =
    match toplets with
    | [] -> Unit
    | toplet :: rest ->
        let desugared_value = desugar_sfexpr (desugar_function toplet.args toplet.value) in
        let desugared_body = desugar_toplets rest in
        Let {
          is_rec = toplet.is_rec;
          name = toplet.name;
          ty = desugar_ty toplet.ty;
          value = desugared_value;
          body = desugared_body;
        }
  in
  desugar_toplets prog

and desugar_sfexpr expr =
  match expr with
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar x -> Var x
  | SIf (e1, e2, e3) -> If (desugar_sfexpr e1, desugar_sfexpr e2, desugar_sfexpr e3)
  | SBop (op, e1, e2) -> Bop (op, desugar_sfexpr e1, desugar_sfexpr e2)
  | SAssert e -> Assert (desugar_sfexpr e)
  | SFun { arg; args; body } ->
      desugar_fun (arg :: args) body
  | SApp (e1, e2) -> App (desugar_sfexpr e1, desugar_sfexpr e2)
  | SLet { is_rec; name; args; ty; value; body } ->
      let desugared_value = desugar_sfexpr (desugar_function args value) in
      Let {
        is_rec;
        name;
        ty = desugar_ty ty;
        value = desugared_value;
        body = desugar_sfexpr body;
      }

and desugar_fun args body =
  match args with
  | [] -> desugar_sfexpr body
  | (arg_name, arg_ty) :: rest ->
      let inner_fun = desugar_fun rest body in
      Fun (arg_name, desugar_ty arg_ty, inner_fun)

and desugar_function args value =
  match args with
  | [] -> value
  | arg :: rest ->
      SFun { arg = arg; args = rest; body = value }

and desugar_ty ty =
  match ty with
  | IntTy -> IntTy
  | BoolTy -> BoolTy
  | UnitTy -> UnitTy
  | FunTy (t1, t2) -> FunTy (desugar_ty t1, desugar_ty t2)

  
let type_of expr =
  let rec type_check (gamma : ty_env) (e : expr) : (ty, error) result =
    match e with
    | Unit ->
        Ok UnitTy
    | True | False ->
        Ok BoolTy
    | Num _ ->
        Ok IntTy
    | Var x ->
        (match Env.find_opt x gamma with
         | Some ty -> Ok ty
         | None -> Error (UnknownVar x))
    | Bop (op, e1, e2) ->
        let (expected_left_ty, expected_right_ty, result_ty) = expected_types_of_bop op in
        type_check gamma e1 >>= fun ty1 ->
        if ty1 = expected_left_ty then
          type_check gamma e2 >>= fun ty2 ->
          if ty2 = expected_right_ty then
            Ok result_ty
          else
            Error (OpTyErrR (op, expected_right_ty, ty2))
        else
          Error (OpTyErrL (op, expected_left_ty, ty1))
    | If (e1, e2, e3) ->
        type_check gamma e1 >>= fun cond_ty ->
        if cond_ty = BoolTy then
          type_check gamma e2 >>= fun then_ty ->
          type_check gamma e3 >>= fun else_ty ->
          if then_ty = else_ty then
            Ok then_ty
          else
            Error (IfTyErr (then_ty, else_ty))
        else
          Error (IfCondTyErr cond_ty)
    | Fun (arg_name, arg_ty, body) ->
        let gamma' = Env.add arg_name arg_ty gamma in
        type_check gamma' body >>= fun body_ty ->
        Ok (FunTy (arg_ty, body_ty))
    | App (e1, e2) ->
        type_check gamma e1 >>= fun fun_ty ->
        (match fun_ty with
         | FunTy (arg_ty, ret_ty) ->
             type_check gamma e2 >>= fun arg_ty_actual ->
             if arg_ty_actual = arg_ty then
               Ok ret_ty
             else
               Error (FunArgTyErr (arg_ty, arg_ty_actual))
         | _ ->
             Error (FunAppTyErr fun_ty))
    | Let { is_rec; name; ty; value; body } ->
        if is_rec then
          let gamma' = Env.add name ty gamma in
          type_check gamma' value >>= fun value_ty ->
          if value_ty = ty then
            type_check gamma' body
          else
            Error (LetTyErr (ty, value_ty))
        else
          type_check gamma value >>= fun value_ty ->
          if value_ty = ty then
            let gamma' = Env.add name ty gamma in
            type_check gamma' body
          else
            Error (LetTyErr (ty, value_ty))
    | Assert e ->
        type_check gamma e >>= fun ty ->
        if ty = BoolTy then
          Ok UnitTy
        else
          Error (AssertTyErr ty)
  in
  type_check Env.empty expr

  let eval expr =
    let rec eval_expr (env : val_env) (e : expr) : value =
      match e with
      | Unit -> VUnit
      | True -> VBool true
      | False -> VBool false
      | Num n -> VNum n
      | Var x ->
          (match Env.find_opt x env with
           | Some v -> v
           | None -> failwith ("Unbound variable " ^ x))
      | Bop (op, e1, e2) ->
          let v1 = eval_expr env e1 in
          let v2 = eval_expr env e2 in
          eval_bop op v1 v2
      | If (e1, e2, e3) ->
          let cond = eval_expr env e1 in
          (match cond with
           | VBool true -> eval_expr env e2
           | VBool false -> eval_expr env e3
           | _ -> failwith "Condition is not a boolean")
      | Fun (arg_name, _, body) ->
          VClos { name = None; arg = arg_name; body; env }
      | App (e1, e2) ->
          let func = eval_expr env e1 in
          let arg_val = eval_expr env e2 in
          (match func with
           | VClos { name; arg; body; env = closure_env } ->
               let env' = Env.add arg arg_val closure_env in
               let env' =
                 match name with
                 | Some fname -> Env.add fname func env'
                 | None -> env'
               in
               eval_expr env' body
           | _ -> failwith "Attempted to call a non-function value")
      | Let { is_rec = false; name; ty = _; value; body } ->
          let value_val = eval_expr env value in
          let env' = Env.add name value_val env in
          eval_expr env' body
          | Let { is_rec = true; name; ty = _; value; body } ->
            (match value with
             | Fun (arg_name, _, body_fun) ->
                 let closure = VClos { name = Some name; arg = arg_name; body = body_fun; env = env } in
                 let env' = Env.add name closure env in
                 eval_expr env' body
             | _ -> failwith "Recursive let must define a function")
             
      | Assert e ->
          let cond = eval_expr env e in
          (match cond with
           | VBool true -> VUnit
           | VBool false -> raise AssertFail
           | _ -> failwith "Assert expression is not a boolean")
    and eval_bop op v1 v2 =
      match (op, v1, v2) with
      | (Add, VNum n1, VNum n2) -> VNum (n1 + n2)
      | (Sub, VNum n1, VNum n2) -> VNum (n1 - n2)
      | (Mul, VNum n1, VNum n2) -> VNum (n1 * n2)
      | (Div, VNum n1, VNum n2) ->
          if n2 = 0 then raise DivByZero else VNum (n1 / n2)
      | (Mod, VNum n1, VNum n2) ->
          if n2 = 0 then raise DivByZero else VNum (n1 mod n2)
      | (Lt, VNum n1, VNum n2) -> VBool (n1 < n2)
      | (Lte, VNum n1, VNum n2) -> VBool (n1 <= n2)
      | (Gt, VNum n1, VNum n2) -> VBool (n1 > n2)
      | (Gte, VNum n1, VNum n2) -> VBool (n1 >= n2)
      | (Eq, VNum n1, VNum n2) -> VBool (n1 = n2)
      | (Neq, VNum n1, VNum n2) -> VBool (n1 <> n2)
      | (And, VBool b1, VBool b2) -> VBool (b1 && b2)
      | (Or, VBool b1, VBool b2) -> VBool (b1 || b2)
      | _ -> failwith "Invalid operands for binary operation"
    in
    eval_expr Env.empty expr
  
    let interp s =
      match parse s with
      | None -> Error ParseErr
      | Some prog ->
          let expr = desugar prog in
          match type_of expr with
          | Ok _ ->
              (try
                 let v = eval expr in
                 Ok v
               with
               | AssertFail -> Error (AssertTyErr BoolTy)
               | DivByZero -> Error (OpTyErrR (Div, IntTy, IntTy)) 
               | Failure msg -> Error (UnknownVar msg)) 
          | Error err -> Error err
    
  
