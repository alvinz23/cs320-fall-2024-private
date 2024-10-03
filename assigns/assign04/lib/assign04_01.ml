

let solve funcs start pred = 
  let rec wrap start funcs pred steps limit = 
    if steps > limit  then 
      (None, funcs) 
  else if pred start then (Some steps, funcs)
  else let newVal = funcs start in wrap newVal funcs pred (steps + 1) limit

in wrap start funcs pred 0 950

let rec checkDuplicate maxVal funcs count infCount start pred= 
  match funcs with 
  | [] -> count < 2 && infCount < 2
  | h :: t -> let (steps, _) = solve h start pred
in match steps with 
  | None -> if (infCount + 1) = 2 then false else checkDuplicate maxVal t count (infCount + 1) start pred 
  | Some s when s = maxVal -> if (count + 1) = 2 then false else checkDuplicate maxVal t (count + 1) infCount start pred
  | Some _ -> checkDuplicate maxVal t count infCount start pred


  let last_function_standing funcs start pred = 
    let rec helpSolve funcs maxVal maxFunc = 
      match funcs with 
      | [] -> if checkDuplicate maxVal funcs 0 0 start pred then Some maxFunc else None  (* Base case: no more functions to check *)
      | h :: t -> 
          let (steps, func) = solve h start pred in  (* Call solve on the current function *)
          match steps with
          | None ->  (* Handle infinite lifespan *)
              if checkDuplicate maxVal funcs 0 0 start pred then Some func else None
          | Some s when s > maxVal ->  (* Update maxVal if current function has longer lifespan *)
              helpSolve t s func  (* Call helpSolve recursively on the rest, using 's' as maxVal *)
          | Some _ ->  
              helpSolve t maxVal maxFunc  (* Continue with the rest of the list without updating maxVal *)
    in helpSolve funcs 0 (fun x -> x)  (* Initial call with default maxVal of 0 and a default identity function for maxFunc *)
  