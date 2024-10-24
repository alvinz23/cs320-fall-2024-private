
(* lib/assign06_02.ml *)

open Utils

let parse toks =
  let rec processTokens tokens exprStack =
    match tokens with
    | [] ->
        (match exprStack with
         | [finalExpr] -> Some finalExpr
         | _ -> None)  
    | currentTok :: remainingToks ->
        (match currentTok with
         | TNum n ->
             processTokens remainingToks (Num n :: exprStack)
         | TAdd ->
             (match exprStack with
              | expr1 :: expr2 :: restStack ->
                  let addedExpr = Add (expr2, expr1) in
                  processTokens remainingToks (addedExpr :: restStack)
              | _ -> None)  
         | TLt ->
             (match exprStack with
              | expr1 :: expr2 :: restStack ->
                  let ltExpr = Lt (expr2, expr1) in
                  processTokens remainingToks (ltExpr :: restStack)
              | _ -> None) 
         | TIte ->
             (match exprStack with
              | expr1 :: expr2 :: expr3 :: restStack ->
                  let iteExpr = Ite (expr3, expr2, expr1) in
                  processTokens remainingToks (iteExpr :: restStack)
              | _ -> None)  
        )
  in
  processTokens toks []
