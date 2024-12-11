
open Utils
open Stdlib320
open My_parser


(* Part 1: Type Inference *)

let parse s = parse s

let apply_subst_type s t =
  let rec go t =
    match t with
    | TUnit | TInt | TFloat | TBool -> t
    | TVar a ->
      (match List.find_opt (fun (x,_) -> x = a) s with
       | Some (_, ty) -> go ty
       | None -> TVar a)
    | TList t' -> TList (go t')
    | TOption t' -> TOption (go t')
    | TPair(t1,t2) -> TPair(go t1, go t2)
    | TFun(t1,t2) -> TFun(go t1, go t2)
  in go t

let free_ty_vars t =
  let rec go acc t =
    match t with
    | TUnit | TInt | TFloat | TBool -> acc
    | TVar a -> if List.mem a acc then acc else a :: acc
    | TList t' -> go acc t'
    | TOption t' -> go acc t'
    | TPair(t1,t2) -> go (go acc t1) t2
    | TFun(t1,t2) -> go (go acc t1) t2
  in go [] t

let instantiate (Forall(vars, t)) =
  let s = List.map (fun v -> (v, TVar(gensym()))) vars in
  apply_subst_type s t

let generalize env t =
  let bindings = Env.to_list env in
  let env_fvs =
    List.fold_left (fun acc (_, Forall(_, ty)) ->
      let all_vars = free_ty_vars ty in
      List.fold_left (fun a x -> if List.mem x a then a else x :: a) acc all_vars
    ) [] bindings
  in
  let fvs = free_ty_vars t in
  let gs = List.filter (fun x -> not (List.mem x env_fvs)) fvs in
  Forall(gs, t)

let rec occurs a t =
  match t with
  | TVar x -> x = a
  | TList t' -> occurs a t'
  | TOption t' -> occurs a t'
  | TPair (t1,t2) -> occurs a t1 || occurs a t2
  | TFun (t1,t2) -> occurs a t1 || occurs a t2
  | _ -> false

let rec unify_one t1 t2 s =
  let t1 = apply_subst_type s t1 in
  let t2 = apply_subst_type s t2 in
  match t1, t2 with
  | a,b when a=b -> Some s
  | TVar x, t
  | t, TVar x ->
    if occurs x t then None else Some ((x,t)::s)
  | TList a, TList b -> unify_one a b s
  | TOption a, TOption b -> unify_one a b s
  | TPair(a1,a2), TPair(b1,b2) ->
    (match unify_one a1 b1 s with
     | None -> None
     | Some s' -> unify_one a2 b2 s')
  | TFun(a1,a2), TFun(b1,b2) ->
    (match unify_one a1 b1 s with
     | None -> None
     | Some s' -> unify_one a2 b2 s')
  | TInt, TFloat | TFloat, TInt -> None
  | _ -> None

let rec unify_constraints s cs =
  match cs with
  | [] -> Some s
  | (t1,t2)::rest ->
    (match unify_one t1 t2 s with
     | None -> None
     | Some s' -> unify_constraints s' rest)

let unify initial_ty constraints =
  match unify_constraints [] constraints with
  | None -> None
  | Some s ->
    let t = apply_subst_type s initial_ty in
    Some (Forall(free_ty_vars t, t))

let lookup_env env x =
  match Env.find_opt x env with
  | Some sc -> instantiate sc
  | None -> failwith ("Unbound variable " ^ x)

let rec infer env e =
  match e with
  | Unit -> (TUnit, [])
  | True -> (TBool, [])
  | False -> (TBool, [])
  | Int _ -> (TInt, [])
  | Float _ -> (TFloat, [])
  | Var x ->
    let t = lookup_env env x in
    (t, [])
  | Annot(e', t_annot) ->
    let (t', c) = infer env e' in
    (t_annot, (t', t_annot)::c)
  | Assert e' ->
    let (te, ce) = infer env e' in
    (TUnit, (te,TBool)::ce)
  | ESome e' ->
    let (t,c) = infer env e' in
    (TOption t, c)
  | ENone ->
    let a = TVar(gensym()) in
    (TOption a, [])
  | Nil ->
    let a = TVar(gensym()) in
    (TList a, [])
  | Bop(op,e1,e2) ->
    let (t1,c1) = infer env e1 in
    let (t2,c2) = infer env e2 in
    let (r, c_op) = infer_bop op t1 t2 in
    (r, c1 @ c2 @ c_op)
  | If(e1,e2,e3) ->
    let (t1,c1)=infer env e1 in
    let (t2,c2)=infer env e2 in
    let (t3,c3)=infer env e3 in
    (t3, c1 @ c2 @ c3 @ [(t1,TBool);(t2,t3)])
  | Fun(x,x_ty,body) ->
    let alpha = match x_ty with Some ty -> ty | None -> TVar(gensym()) in
    let env' = Env.add x (Forall([], alpha)) env in
    let (tb, cb) = infer env' body in
    (TFun(alpha,tb), cb)
  | App(e1,e2) ->
    let (t1,c1)=infer env e1 in
    let (t2,c2)=infer env e2 in
    let a = TVar(gensym()) in
    (a, c1 @ c2 @ [(t1, TFun(t2,a))])
  | Let {is_rec=false;name;value;body} ->
    let (tv, cv) = infer env value in
    let env' = Env.add name (Forall([],tv)) env in
    let (tb, cb) = infer env' body in
    (tb, cv @ cb)
  | Let {is_rec=true;name;value;body} ->
    let alpha = TVar(gensym()) in
    let beta = TVar(gensym()) in
    let env' = Env.add name (Forall([],TFun(alpha,beta))) env in
    let (tv, cv) = infer env' value in
    let env'' = Env.add name (Forall([],tv)) env' in
    let (tb, cb) = infer env'' body in
    (tb, cv @ cb @ [(tv,TFun(alpha,beta))])
  | PairMatch {matched;fst_name;snd_name;case} ->
    let (tm, cm)= infer env matched in
    let a = TVar(gensym()) in
    let b = TVar(gensym()) in
    let env' = Env.add fst_name (Forall([],a)) (Env.add snd_name (Forall([],b)) env) in
    let (tc, cc) = infer env' case in
    (tc, cm @ cc @ [(tm,TPair(a,b))])
  | ListMatch {matched;hd_name;tl_name;cons_case;nil_case} ->
    let (tm,cm) = infer env matched in
    let a = TVar(gensym()) in
    let env_cons = Env.add hd_name (Forall([],a)) (Env.add tl_name (Forall([],TList a)) env) in
    let (tc1,cc1) = infer env_cons cons_case in
    let (tc2,cc2) = infer env nil_case in
    (tc2, cm @ cc1 @ cc2 @ [(tm,TList a);(tc1,tc2)])
  | OptMatch {matched;some_name;some_case;none_case} ->
    let (tm,cm) = infer env matched in
    let a = TVar(gensym()) in
    let env_some = Env.add some_name (Forall([],a)) env in
    let (tsc, csc) = infer env_some some_case in
    let (tnc, cnc) = infer env none_case in
    (tnc, cm @ csc @ cnc @ [(tm,TOption a);(tsc,tnc)])

and infer_bop op t1 t2 =
  match op with
  | Add | Sub | Mul | Div | Mod ->
    (TInt,[(t1,TInt);(t2,TInt)])
  | AddF| SubF | MulF | DivF | PowF ->
    (TFloat,[(t1,TFloat);(t2,TFloat)])
  | Lt | Lte | Gt | Gte | Eq | Neq ->
    (TBool,[(t1,t2)])
  | And | Or ->
    (TBool,[(t1,TBool);(t2,TBool)])
  | Cons ->
    (TList t1,[(t2,TList t1)])
  | Concat ->
    let a = TVar(gensym()) in
    (TList a,[(t1,TList a);(t2,TList a)])
  | Comma ->
    (TPair(t1,t2),[])

let type_of env e =
  let (t,c) = infer env e in
  match unify t c with
  | None -> None
  | Some (Forall(_, ty)) -> Some (generalize env ty)


exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals


let rec eval_expr env e =
  match e with
  | Unit -> VUnit
  | True -> VBool true
  | False -> VBool false
  | Int n -> VInt n
  | Float f -> VFloat f
  | Var x ->
    (match Env.find_opt x env with
     | Some v -> v
     | None -> failwith ("Unbound variable at runtime: "^x))
  | Annot (e,_) -> eval_expr env e
  | Assert e ->
    (match eval_expr env e with
     | VBool true -> VUnit
     | VBool false -> raise AssertFail
     | _ -> failwith "Type error in assert")
  | ESome e ->
    VSome (eval_expr env e)
  | ENone -> VNone
  | Nil -> VList []
  | Bop(op,e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    eval_bop op v1 v2
  | If(e1,e2,e3) ->
    (match eval_expr env e1 with
     | VBool true -> eval_expr env e2
     | VBool false -> eval_expr env e3
     | _ -> failwith "Type error in if condition")
  | Fun(x,_,body) ->
    VClos {name=None;arg=x;body;env=env}
  | App(e1,e2) ->
    let v1 = eval_expr env e1 in
    let v2 = eval_expr env e2 in
    (match v1 with
     | VClos{name=None;arg;body;env=clos_env} ->
       eval_expr (Env.add arg v2 clos_env) body
     | VClos{name=Some f;arg;body;env=clos_env} ->
       let rec_cl = VClos{name=Some f;arg;body;env=clos_env} in
       let env' = Env.add f rec_cl (Env.add arg v2 clos_env) in
       eval_expr env' body
     | _ -> failwith "Application of non-function")
  | Let {is_rec=false;name;value;body} ->
    let v = eval_expr env value in
    eval_expr (Env.add name v env) body
  | Let {is_rec=true;name;value;body} ->
    (match eval_expr env value with
     | VClos{name=None;arg;body=fun_body;env=clos_env} ->
       let rec_cl = VClos{name=Some name; arg; body=fun_body; env=clos_env} in
       eval_expr (Env.add name rec_cl env) body
     | _ -> raise RecWithoutArg)
  | PairMatch{matched;fst_name;snd_name;case} ->
    (match eval_expr env matched with
     | VPair(v1,v2) ->
       let env' = Env.add fst_name v1 (Env.add snd_name v2 env) in
       eval_expr env' case
     | _ -> failwith "Pattern match error on pair")
  | ListMatch{matched;hd_name;tl_name;cons_case;nil_case} ->
    (match eval_expr env matched with
     | VList(h::t) ->
       eval_expr (Env.add hd_name h (Env.add tl_name (VList t) env)) cons_case
     | VList [] ->
       eval_expr env nil_case
     | _ -> failwith "Pattern match error on list")
  | OptMatch{matched;some_name;some_case;none_case} ->
    (match eval_expr env matched with
     | VSome v ->
       eval_expr (Env.add some_name v env) some_case
     | VNone ->
       eval_expr env none_case
     | _ -> failwith "Pattern match error on option")

and eval_bop op v1 v2 =
  let int_op f = match v1,v2 with
    | VInt a, VInt b -> VInt (f a b)
    | _ -> failwith "Type error in int_op"
  in
  let float_op f = match v1,v2 with
    | VFloat a, VFloat b -> VFloat (f a b)
    | _ -> failwith "Type error in float_op"
  in
  let compare_op f =
    let rec cmp v1 v2 =
      match v1,v2 with
      | VInt a, VInt b -> compare a b
      | VFloat a, VFloat b ->
        compare a b
      | VBool a, VBool b -> compare a b
      | VUnit,VUnit -> 0
      | VList l1, VList l2 -> cmp_list l1 l2
      | VPair(x1,y1), VPair(x2,y2) ->
        let c = cmp x1 x2 in
        if c=0 then cmp y1 y2 else c
      | VNone, VNone -> 0
      | VSome v1, VSome v2 -> cmp v1 v2
      | VClos _,_ | _,VClos _ -> raise CompareFunVals
      | _ ->
      
        let tag v = match v with
          | VUnit -> 0
          | VBool _ -> 1
          | VInt _ -> 2
          | VFloat _ -> 3
          | VList _ -> 4
          | VPair _ -> 5
          | VNone -> 6
          | VSome _ -> 7
          | VClos _ -> 8
        in
        compare (tag v1) (tag v2)
    and cmp_list l1 l2 =
      match l1,l2 with
      | [],[] -> 0
      | [],_ -> -1
      | _,[] -> 1
      | x::xs,y::ys ->
        let c = cmp x y in
        if c=0 then cmp_list xs ys else c
    in
    let c = cmp v1 v2 in
    VBool (f c 0)
  in
  match op with
  | Add -> int_op (fun a b -> a+b)
  | Sub -> int_op (fun a b -> a-b)
  | Mul -> int_op (fun a b -> a*b)
  | Div ->
    (match v1,v2 with
     | VInt _, VInt 0 -> raise DivByZero
     | VInt a, VInt b -> VInt(a/b)
     | _ -> failwith "Type error in Div")
  | Mod ->
    (match v1,v2 with
     | VInt _, VInt 0 -> raise DivByZero
     | VInt a, VInt b -> VInt(a mod b)
     | _ -> failwith "Type error in Mod")
  | AddF -> float_op (fun a b -> a+.b)
  | SubF -> float_op (fun a b -> a-.b)
  | MulF -> float_op (fun a b -> a*.b)
  | DivF -> float_op (fun a b -> a/.b)
  | PowF -> float_op (fun a b -> a ** b)
  | Lt -> compare_op (fun c0 _ -> c0<0)
  | Lte-> compare_op (fun c0 _ -> c0<=0)
  | Gt -> compare_op (fun c0 _ -> c0>0)
  | Gte-> compare_op (fun c0 _ -> c0>=0)
  | Eq -> compare_op (fun c0 _ -> c0=0)
  | Neq-> compare_op (fun c0 _ -> c0<>0)
  | And -> (match v1,v2 with VBool a,VBool b->VBool(a&&b)|_->failwith"Type error in &&")
  | Or -> (match v1,v2 with VBool a,VBool b->VBool(a||b)|_->failwith"Type error in ||")
  | Cons -> (match v2 with
    | VList l -> VList (v1::l)
    | _ -> failwith "Type error in cons")
  | Concat ->
    let rec concat l1 l2 = match l1 with
      | [] -> l2
      | h::t -> h::concat t l2
    in
    (match v1,v2 with
     | VList l1, VList l2 -> VList (concat l1 l2)
     | _ -> failwith "Type error in @")
  | Comma -> VPair(v1,v2)

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] -> Let {is_rec;name;value;body=Var name}
    | {is_rec;name;value}::ls -> Let{is_rec;name;value;body=nest ls}
  in eval_expr Env.empty (nest p)

let type_check p =
  let rec go env = function
    | [] -> Some (Forall([], TUnit))
    | {is_rec; name; value}::ls ->
      let exp = Let{is_rec;name;value;body=Var name} in
      match type_of env exp with
      | None -> None
      | Some sc ->
        let env = Env.add name sc env in
        go env ls
  in go Env.empty p

let interp s =
  match parse s with
  | None -> Error ParseError
  | Some prog ->
    (match type_check prog with
     | None -> Error TypeError
     | Some sc ->
       let v = eval prog in
       Ok(v, sc))