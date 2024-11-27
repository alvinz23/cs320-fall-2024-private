
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

      
      let tail = function
      | [] -> failwith "dne" 
      | _ :: t -> t
    
    let head = function
      | [] -> failwith "dne"
      | h :: _ -> h
    
    let desugar : prog -> expr = fun program ->
      let rec transform_sfexpr (sf_expr : sfexpr) : expr =
        match sf_expr with
        | SUnit -> Unit
        | STrue -> True
        | SFalse -> False
        | SNum n -> Num n
        | SVar x -> Var x
        | SBop (op, e1, e2) -> Bop (op, transform_sfexpr e1, transform_sfexpr e2)
        | SIf (cond, then_expr, else_expr) ->
            If (transform_sfexpr cond, transform_sfexpr then_expr, transform_sfexpr else_expr)
        | SAssert expr -> Assert (transform_sfexpr expr)
        | SFun { arg = (x, t); args = []; body } ->
            Fun (x, t, transform_sfexpr body)
        | SFun { arg = (x, t); args = more_args; body } ->
            let next_arg = head more_args in
            let remaining_args = tail more_args in
            Fun (x, t, transform_sfexpr (SFun { arg = next_arg; args = remaining_args; body }))
        | SLet { is_rec; name; args = []; ty; value; body } ->
            Let { is_rec; name; ty; value = transform_sfexpr value; body = transform_sfexpr body }
        | SLet { is_rec; name; args = first_arg :: rest_args; ty; value; body } ->
            let function_type = List.fold_right (fun (_, t) acc -> FunTy (t, acc)) rest_args ty in
            Let
              {
                is_rec;
                name;
                ty = FunTy (snd first_arg, function_type);
                value = transform_sfexpr (SFun { arg = first_arg; args = rest_args; body = value });
                body = transform_sfexpr body;
              }
        | SApp (e1, e2) -> App (transform_sfexpr e1, transform_sfexpr e2)
      in
    
      let rec transform_prog_items (prog_items : prog) =
        match prog_items with
        | [] -> Unit
        | { is_rec; name; args; ty; value } :: rest_items ->
            let transformed_value =
              if args = [] then value
              else SFun { arg = head args; args = tail args; body = value }
            in
            let function_type = List.fold_right (fun (_, t) acc -> FunTy (t, acc)) args ty in
            Let
              {
                is_rec;
                name;
                ty = if args = [] then ty else function_type;
                value = transform_sfexpr transformed_value;
                body = transform_prog_items rest_items;
              }
      in
    
      transform_prog_items program
    
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
          (match op with
           | And ->
               (match eval_expr env e1 with
                | VBool false -> VBool false  (* Short-circuit *)
                | VBool true -> eval_expr env e2
                | _ -> failwith "Type error in And operation")
           | Or ->
               (match eval_expr env e1 with
                | VBool true -> VBool true  (* Short-circuit *)
                | VBool false -> eval_expr env e2
                | _ -> failwith "Type error in Or operation")
           | _ ->
               let v1 = eval_expr env e1 in
               let v2 = eval_expr env e2 in
               eval_bop op v1 v2)
      | If (e1, e2, e3) ->
          (match eval_expr env e1 with
           | VBool true -> eval_expr env e2
           | VBool false -> eval_expr env e3
           | _ -> failwith "Type error in If condition")
      | Fun (arg_name, _, body) ->
          VClos { name = None; arg = arg_name; body = body; env = env }
      | App (e1, e2) ->
          (match eval_expr env e1 with
           | VClos { name = None; arg; body; env = closure_env } ->
               let arg_val = eval_expr env e2 in
               let new_env = Env.add arg arg_val closure_env in
               eval_expr new_env body
           | VClos { name = Some fname; arg; body; env = closure_env } ->
               let arg_val = eval_expr env e2 in
               let recursive_env = Env.add fname (VClos { name = Some fname; arg; body; env = closure_env }) closure_env in
               let new_env = Env.add arg arg_val recursive_env in
               eval_expr new_env body
           | _ -> failwith "Type error in function application")
      | Let { is_rec = false; name; ty = _; value; body } ->
          let value_val = eval_expr env value in
          let new_env = Env.add name value_val env in
          eval_expr new_env body
      | Let { is_rec = true; name; ty = _; value; body } ->
          (match value with
           | Fun (arg_name, _, body_fun) ->
               let rec_closure = VClos { name = Some name; arg = arg_name; body = body_fun; env = env } in
               let new_env = Env.add name rec_closure env in
               eval_expr new_env body
           | _ -> failwith "Type error in recursive Let binding")
      | Assert e ->
          (match eval_expr env e with
           | VBool true -> VUnit
           | VBool false -> raise AssertFail
           | _ -> failwith "Type error in Assert")
    and eval_bop op v1 v2 =
      match (op, v1, v2) with
      | (Add, VNum n1, VNum n2) -> VNum (n1 + n2)
      | (Sub, VNum n1, VNum n2) -> VNum (n1 - n2)
      | (Mul, VNum n1, VNum n2) -> VNum (n1 * n2)
      | (Div, VNum _, VNum 0) -> raise DivByZero
      | (Div, VNum n1, VNum n2) -> VNum (n1 / n2)
      | (Mod, VNum _, VNum 0) -> raise DivByZero
      | (Mod, VNum n1, VNum n2) -> VNum (n1 mod n2)
      | (Lt, VNum n1, VNum n2) -> VBool (n1 < n2)
      | (Lte, VNum n1, VNum n2) -> VBool (n1 <= n2)
      | (Gt, VNum n1, VNum n2) -> VBool (n1 > n2)
      | (Gte, VNum n1, VNum n2) -> VBool (n1 >= n2)
      | (Eq, VNum n1, VNum n2) -> VBool (n1 = n2)
      | (Neq, VNum n1, VNum n2) -> VBool (n1 <> n2)
      | (Eq, VBool b1, VBool b2) -> VBool (b1 = b2)
      | (Neq, VBool b1, VBool b2) -> VBool (b1 <> b2)
      | (Eq, VUnit, VUnit) -> VBool true
      | (Neq, VUnit, VUnit) -> VBool false
      | _ -> failwith "Type error in binary operation"
    in
    eval_expr Env.empty expr
  
    let interp (s : string) : (value, error) result =
      match parse s with
      | Some prog ->
          let expr = desugar prog in
          (match type_of expr with
          | Ok _ -> Ok (eval expr)
          | Error err -> Error err)
          |None -> Error ParseErr