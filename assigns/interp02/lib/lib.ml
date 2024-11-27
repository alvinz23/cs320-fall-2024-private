
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

      
  
      let desugar : prog -> expr = fun program ->
  let rec transform_expr (surface_expr : sfexpr) : expr =
    match surface_expr with
    | SUnit -> Unit
    | STrue -> True
    | SFalse -> False
    | SNum number -> Num number
    | SVar variable -> Var variable
    | SBop (binary_op, left, right) ->
        Bop (binary_op, transform_expr left, transform_expr right)
    | SIf (condition, then_branch, else_branch) ->
        If (transform_expr condition, transform_expr then_branch, transform_expr else_branch)
    | SAssert assertion_expr -> Assert (transform_expr assertion_expr)
    | SFun { arg = (arg_name, arg_type); args = []; body } ->
        Fun (arg_name, arg_type, transform_expr body)
    | SFun { arg = (arg_name, arg_type); args = args_list; body } ->
        (match args_list with
         | [] -> Fun (arg_name, arg_type, transform_expr body)
         | first_arg :: rest_args ->
             transform_expr (SFun { arg = first_arg; args = rest_args; body }))
    | SLet { is_rec = is_recursive; name = binding_name; args = []; ty = binding_type; value; body } ->
        Let
          {
            is_rec = is_recursive;
            name = binding_name;
            ty = binding_type;
            value = transform_expr value;
            body = transform_expr body;
          }
    | SLet { is_rec = is_recursive; name = binding_name; args = binding_args; ty; value; body } ->
        (match binding_args with
         | [] -> transform_expr (SLet { is_rec = is_recursive; name = binding_name; args = []; ty; value; body })
         | first_arg :: rest_args ->
             Let
               {
                 is_rec = is_recursive;
                 name = binding_name;
                 ty =
                   List.fold_right
                     (fun (_, arg_type) acc -> FunTy (arg_type, acc))
                     rest_args ty;
                 value =
                   transform_expr
                     (SFun { arg = first_arg; args = rest_args; body = value });
                 body = transform_expr body;
               })
    | SApp (function_expr, argument_expr) ->
        App (transform_expr function_expr, transform_expr argument_expr)
  in

  let rec process topLvl =
    match topLvl with
    | [] -> Unit
    | { is_rec = is_recursive; name = binding_name; args = binding_args; ty = binding_type; value } :: remaining_bindings ->
        (match binding_args with
         | [] ->
             Let
               {
                 is_rec = is_recursive;
                 name = binding_name;
                 ty = binding_type;
                 value = transform_expr value;
                 body = process remaining_bindings;
               }
         | first_arg :: rest_args ->
             Let
               {
                 is_rec = is_recursive;
                 name = binding_name;
                 ty =
                   List.fold_right
                     (fun (_, arg_type) acc -> FunTy (arg_type, acc))
                     binding_args binding_type;
                 value =
                   transform_expr
                     (SFun { arg = first_arg; args = rest_args; body = value });
                 body = process remaining_bindings;
               })
  in
  process program

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
    
  
