(* placeholder *)
let rec pow n k = 
  if k = 1 then
    n 
  else if k = 0 then 
    1
  else n * pow n (k-1)

