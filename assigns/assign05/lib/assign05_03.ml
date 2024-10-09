

type ident = string

type ty = 
  | Unit
  | Arr of ty * ty

type expr = 
  | Var of ident
  | Fun of ident * ty * expr
  | App of expr * expr

let rec type_of gamma e =
  match e with
  | Var name ->
      (match List.assoc_opt name gamma with
       | Some t -> Some t
       | None -> None)
  | Fun (param, paramType, bod) ->
      let newGamma = (param, paramType) :: gamma in
      (match type_of newGamma bod with
       | Some bodType -> Some (Arr (paramType, bodType))
       | None -> None)
  | App (func, argument) ->
      (match type_of gamma func with
       | Some funcType ->
           (match funcType with
      | Arr (expectedType, retType) ->
                (match type_of gamma argument with
                 | Some actualRet ->
                     if actualRet = expectedType then Some retType
                     else None
                 | None -> None)
      | _ -> None)
       | None -> None)
