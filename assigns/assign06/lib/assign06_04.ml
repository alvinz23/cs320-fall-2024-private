



open Utils
  let rec eval expr =
    match expr with
    | Num n -> VNum n
    | Add (e1, e2) ->
        (match (eval e1, eval e2) with
         | (VNum n1, VNum n2) -> VNum (n1 + n2)
         | _ -> VNum 0)
    | Lt (e1, e2) ->
        (match (eval e1, eval e2) with
         | (VNum n1, VNum n2) -> VBool (n1 < n2)
         | _ -> VBool false)
    | Ite (cond, then_branch, else_branch) ->
        (match eval cond with
         | VBool true -> eval then_branch
         | VBool false -> eval else_branch
         | _ -> VBool false)