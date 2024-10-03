

let solve funcs start pred = 
  let rec wrap start funcs pred steps limit = 
    if steps > limit then 
      (None, funcs) 
    else if pred start then (Some steps, funcs)
    else 
      let newVal = funcs start in 
      wrap newVal funcs pred (steps + 1) limit
  in wrap start funcs pred 0 9999999999

let rec checkDuplicate maxVal funcs count infCount start pred = 
  match funcs with 
  | [] -> count < 2 && infCount < 2
  | h :: t -> 
      let (steps, _) = solve h start pred in
      match steps with 
      | None -> 
          if (infCount + 1) = 2 then false 
          else checkDuplicate maxVal t count (infCount + 1) start pred 
      | Some s when s = maxVal -> 
          if (count + 1) = 2 then false 
          else checkDuplicate maxVal t (count + 1) infCount start pred
      | Some _ -> 
          checkDuplicate maxVal t count infCount start pred


let last_function_standing funcs start pred =
  if List.length funcs = 0 then None 
  else
    let orig_funcs = funcs in 
    let rec helpSolve funcs maxVal maxFunc = 
      match funcs with 
      | [] -> 
          if checkDuplicate maxVal orig_funcs 0 0 start pred then Some maxFunc else None
      | h :: t -> 
          let (steps, func) = solve h start pred in 
          match steps with
          | None -> 
              helpSolve t maxVal maxFunc  
          | Some s when s > maxVal ->  
              helpSolve t s func  
          | Some s when s = maxVal ->  
              helpSolve t maxVal maxFunc
          | Some _ ->  
              helpSolve t maxVal maxFunc  
    in helpSolve funcs (-1) (fun x -> x) 
