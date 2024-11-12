open Utils
open Stdlib320

type 'a myref = { mutable contents : 'a }

let myref x = { contents = x }
let myget r = r.contents
let myset r x = r.contents <- x

let gensym_var =
  let count = myref 0 in
  fun () ->
    let n = myget count in
    myset count (n + 1);
    "x" ^ string_of_int n

let parse : string -> prog option = 
fun s -> My_parser.parse s 

let rec occurs_in_value y v =
  match v with
  | VNum _ -> false
  | VBool _ -> false
  | VUnit -> false
  | VFun (x, e) ->
      if x = y then false else occurs_in_expr y e

and occurs_in_expr y e =
  match e with
  | Num _ | True | False | Unit -> false
  | Var z -> z = y
  | Bop (_, e1, e2) -> occurs_in_expr y e1 || occurs_in_expr y e2
  | If (e1, e2, e3) -> occurs_in_expr y e1 || occurs_in_expr y e2 || occurs_in_expr y e3
  | Let (z, e1, e2) ->
      occurs_in_expr y e1 ||
      if z = y then false else occurs_in_expr y e2
  | Fun (z, e1) ->
      if z = y then false else occurs_in_expr y e1
  | App (e1, e2) -> occurs_in_expr y e1 || occurs_in_expr y e2

let value_to_expr v =
  match v with
  | VNum n -> Num n
  | VBool b -> if b then True else False
  | VUnit -> Unit
  | VFun (x, e) -> Fun (x, e)

let rec subst_var y y' e =
  match e with
  | Num _ | True | False | Unit -> e
  | Var z -> if z = y then Var y' else Var z
  | Bop (op, e1, e2) -> Bop (op, subst_var y y' e1, subst_var y y' e2)
  | If (e1, e2, e3) -> If (subst_var y y' e1, subst_var y y' e2, subst_var y y' e3)
  | Let (z, e1, e2) ->
      let e1' = subst_var y y' e1 in
      if z = y then
        Let (z, e1', e2)
      else
        Let (z, e1', subst_var y y' e2)
  | Fun (z, e1) ->
      if z = y then
        Fun (z, e1)
      else
        Fun (z, subst_var y y' e1)
  | App (e1, e2) -> App (subst_var y y' e1, subst_var y y' e2)

let rec subst v x e =
  match e with
  | Num _ | True | False | Unit -> e
  | Var y -> if y = x then value_to_expr v else e
  | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)
  | If (e1, e2, e3) -> If (subst v x e1, subst v x e2, subst v x e3)
  | Let (y, e1, e2) ->
      if y = x then
        Let (y, subst v x e1, e2)
      else if occurs_in_value y v then
        let y' = gensym_var () in
        let e2' = subst_var y y' e2 in
        Let (y', subst v x e1, subst v x e2')
      else
        Let (y, subst v x e1, subst v x e2)
  | Fun (y, e1) ->
      if y = x then
        Fun (y, e1)
      else if occurs_in_value y v then
        let y' = gensym_var () in
        let e1' = subst_var y y' e1 in
        Fun (y', subst v x e1')
      else
        Fun (y, subst v x e1)
  | App (e1, e2) -> App (subst v x e1, subst v x e2)


let rec eval e =
  match e with
  | Num n -> Ok (VNum n)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit

  | Var x -> Error (UnknownVar x)

  | Bop (Add, e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match v1, v2 with
      | Error e, _ -> Error e
      | _, Error e -> Error e
      | Ok (VNum n1), Ok (VNum n2) -> Ok (VNum (n1 + n2))
      | _, _ -> Error (InvalidArgs Add))

  | Bop (Sub, e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match v1, v2 with
      | Error e, _ -> Error e
      | _, Error e -> Error e
      | Ok (VNum n1), Ok (VNum n2) -> Ok (VNum (n1 - n2))
      | _, _ -> Error (InvalidArgs Sub))

  | Bop (Mul, e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match v1, v2 with
      | Error e, _ -> Error e
      | _, Error e -> Error e
      | Ok (VNum n1), Ok (VNum n2) -> Ok (VNum (n1 * n2))
      | _, _ -> Error (InvalidArgs Mul))

  | Bop (Div, e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match v1, v2 with
      | Error e, _ -> Error e
      | _, Error e -> Error e
      | Ok (VNum n1), Ok (VNum n2) ->
          if n2 = 0 then Error DivByZero
          else Ok (VNum (n1 / n2))
      | _, _ -> Error (InvalidArgs Div))

  | Bop (Mod, e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match v1, v2 with
      | Error e, _ -> Error e
      | _, Error e -> Error e
      | Ok (VNum n1), Ok (VNum n2) ->
          if n2 = 0 then Error DivByZero
          else Ok (VNum (n1 mod n2))
      | _, _ -> Error (InvalidArgs Mod))

  | Bop (And, e1, e2) ->
      let v1 = eval e1 in
      (match v1 with
      | Error e -> Error e
      | Ok (VBool false) -> Ok (VBool false)
      | Ok (VBool true) ->
          let v2 = eval e2 in
          (match v2 with
          | Error e -> Error e
          | Ok (VBool b) -> Ok (VBool b)
          | _ -> Error (InvalidArgs And))
      | _ -> Error (InvalidArgs And))

  | Bop (Or, e1, e2) ->
      let v1 = eval e1 in
      (match v1 with
      | Error e -> Error e
      | Ok (VBool true) -> Ok (VBool true)
      | Ok (VBool false) ->
          let v2 = eval e2 in
          (match v2 with
          | Error e -> Error e
          | Ok (VBool b) -> Ok (VBool b)
          | _ -> Error (InvalidArgs Or))
      | _ -> Error (InvalidArgs Or))

  | Bop (Lt, e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match v1, v2 with
      | Error e, _ -> Error e
      | _, Error e -> Error e
      | Ok (VNum n1), Ok (VNum n2) -> Ok (VBool (n1 < n2))
      | _, _ -> Error (InvalidArgs Lt))


  | Bop (Lte, e1, e2) ->
    let v1 = eval e1 in
    let v2 = eval e2 in
    (match v1, v2 with
    | Error e, _ -> Error e
    | _, Error e -> Error e
    | Ok (VNum n1), Ok (VNum n2) -> Ok (VBool (n1 <= n2))
    | _, _ -> Error (InvalidArgs Lte))

| Bop (Gt, e1, e2) ->
    let v1 = eval e1 in
    let v2 = eval e2 in
    (match v1, v2 with
    | Error e, _ -> Error e
    | _, Error e -> Error e
    | Ok (VNum n1), Ok (VNum n2) -> Ok (VBool (n1 > n2))
    | _, _ -> Error (InvalidArgs Gt))

| Bop (Gte, e1, e2) ->
    let v1 = eval e1 in
    let v2 = eval e2 in
    (match v1, v2 with
    | Error e, _ -> Error e
    | _, Error e -> Error e
    | Ok (VNum n1), Ok (VNum n2) -> Ok (VBool (n1 >= n2))
    | _, _ -> Error (InvalidArgs Gte))


 | Bop (Eq, e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match v1, v2 with
      | Error e, _ -> Error e
      | _, Error e -> Error e
      | Ok val1, Ok val2 ->
          Ok (VBool (val1 = val2)))

  | Bop (Neq, e1, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match v1, v2 with
      | Error e, _ -> Error e
      | _, Error e -> Error e
      | Ok val1, Ok val2 ->
          Ok (VBool (val1 <> val2))
      )

  | If (e1, e2, e3) ->
      let cond = eval e1 in
      (match cond with
      | Error e -> Error e
      | Ok (VBool true) -> eval e2
      | Ok (VBool false) -> eval e3
      | _ -> Error InvalidIfCond)

  | Let (x, e1, e2) ->
      let v1 = eval e1 in
      (match v1 with
      | Error e -> Error e
      | Ok v ->
          let e2' = subst v x e2 in
          eval e2')

  | Fun (x, e) -> Ok (VFun (x, e))

  | App (e1, e2) ->
      let v1 = eval e1 in
      (match v1 with
      | Error e -> Error e
      | Ok (VFun (x, body)) ->
          let v2 = eval e2 in
          (match v2 with
          | Error e -> Error e
          | Ok arg ->
              let body' = subst arg x body in
              eval body')
      | _ -> Error InvalidApp)

      
let interp (s : string) : (value, error) result =
  match parse s with
  | Some expr -> eval expr
  | None -> Error ParseFail



