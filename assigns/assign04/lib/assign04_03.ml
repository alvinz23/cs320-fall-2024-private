
open Assign04_02  
type value = 
  | VNum of int
  | VBool of bool

let rec eval (e : expr) : value = 
  match e with
  | True -> VBool true 
  | False -> VBool false 
  | Num n -> VNum n 
  | Or (e1, e2) -> 
      (match eval e1, eval e2 with
       | VBool true, _ | _, VBool true -> VBool true
       | VBool false, VBool false -> VBool false
       | _ -> VBool false)
  | Add (e1, e2) -> 
      (match eval e1, eval e2 with
       | VNum e1, VNum e2 -> VNum (e1 + e2)
       | _ -> VBool false)
  | IfThenElse (e1, e2, e3) -> 
      (match eval e1 with
       | VBool true -> eval e2
       | VBool false -> eval e3
       | _ -> VBool false)
