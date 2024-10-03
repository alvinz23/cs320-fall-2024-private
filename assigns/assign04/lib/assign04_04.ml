
type ident = string 

type expr' =        
  | True
  | False
  | Num of int
  | Var of ident
  | Let of ident * expr' * expr' 
  | Add of expr' * expr'
  | Or of expr' * expr'
  | IfThenElse of expr' * expr' * expr'

type ty' =         
  | Int
  | Bool

type context = (ident * ty') list  

let rec checkType (ctx: context) (variableName: ident) : ty' option =
  match ctx with
  | [] -> Some Bool 
  | (contextVarName, contextVarType) :: remainingContext ->
      if variableName = contextVarName then
        Some contextVarType  
      else
        checkType remainingContext variableName  

let rec type_of' (ctx: context) (expression: expr') : ty' option =
  match expression with
  | Num _ -> Some Int
  | False -> Some Bool
  | True -> Some Bool
  | Add (expr1, expr2) ->
      (match type_of' ctx expr1, type_of' ctx expr2 with
       | Some Int, Some Int -> Some Int
       | _ -> None)
  | Or (expr1, expr2) ->
      (match type_of' ctx expr1, type_of' ctx expr2 with
       | Some Bool, Some Bool -> Some Bool
       | _ -> None)
  | IfThenElse (conditionExpr, thenExpr, elseExpr) ->
      (match type_of' ctx conditionExpr, type_of' ctx thenExpr, type_of' ctx elseExpr with
       | Some Bool, Some thenType, Some elseType when thenType = elseType -> Some thenType
       | _ -> None)
  | Var variableName ->
  checkType ctx variableName  
  | Let (variableName, expr1, expr2) ->
    (match type_of' ctx expr1 with
     | Some type1 ->
         let extendedContext = (variableName, type1) :: ctx in
         type_of' extendedContext expr2  
     | None -> None)
