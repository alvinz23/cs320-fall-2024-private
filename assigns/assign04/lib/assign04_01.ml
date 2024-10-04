

let solve funcs start pred = 
  let rec wrap start funcs pred steps limit = 
    if steps > limit then 
      (None, funcs) 
    else if pred start then (Some steps, funcs)
    else 
      let newVal = funcs start in 
      wrap newVal funcs pred (steps + 1) limit
  in wrap start funcs pred 0 9999

let rec checkDuplicate maxVal funcs count infCount start pred hasInfinite = 
  match funcs with 
  | [] -> 
      if hasInfinite then infCount < 2 else count < 2
  | h :: t -> 
      let (steps, _) = solve h start pred in
      match steps with 
      | None -> 
          if hasInfinite then
            if (infCount + 1) = 2 then false 
            else checkDuplicate maxVal t count (infCount + 1) start pred hasInfinite
          else
            checkDuplicate maxVal t count (infCount + 1) start pred hasInfinite
      | Some _ when hasInfinite -> 
          checkDuplicate maxVal t count infCount start pred hasInfinite
      | Some s when s = maxVal -> 
          if (count + 1) = 2 then false 
          else checkDuplicate maxVal t (count + 1) infCount start pred hasInfinite
      | Some _ -> 
          checkDuplicate maxVal t count infCount start pred hasInfinite

let last_function_standing funcs start pred =
  if List.length funcs = 0 then None 
  else
    let orig_funcs = funcs in 
    let rec helpSolve funcs maxVal maxFunc hasInfinite = 
      match funcs with 
      | [] -> 
          if checkDuplicate maxVal orig_funcs 0 0 start pred hasInfinite then Some maxFunc else None
      | h :: t -> 
          let (steps, func) = solve h start pred in 
          match steps with
          | None -> 
              helpSolve t maxVal func true
          | Some _ when hasInfinite ->  
              helpSolve t maxVal maxFunc hasInfinite
          | Some s when s > maxVal ->  
              helpSolve t s func hasInfinite  
          | Some s when s = maxVal ->  
              helpSolve t maxVal maxFunc hasInfinite
          | Some _ ->  
              helpSolve t maxVal maxFunc hasInfinite  
    in helpSolve funcs (-1) (fun x -> x) false
