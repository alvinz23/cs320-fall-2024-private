


open Utils

let rec type_of expr =
  match expr with
  | Num _ ->
      Some TInt
  | Add (leftExpr, rightExpr) ->
      (match (type_of leftExpr, type_of rightExpr) with
       | (Some TInt, Some TInt) -> Some TInt
       | _ -> None)  
  | Lt (leftExpr, rightExpr) ->
      (match (type_of leftExpr, type_of rightExpr) with
       | (Some TInt, Some TInt) -> Some TBool
       | _ -> None)  
  | Ite (condExpr, thenExpr, elseExpr) ->
      (match type_of condExpr with
       | Some TBool ->
           (match (type_of thenExpr, type_of elseExpr) with
            | (Some t1, Some t2) when t1 = t2 -> Some t1
            | _ -> None)  
       | _ -> None)  
