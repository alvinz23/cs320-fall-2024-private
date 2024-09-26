
let mk_unique_keys (lst : (string *  int)list) =
  let rec solve (lst1 :(string * int)list) (res : (string * int) list) (seen : string list)  = 
    match lst1 with 
    | [] -> res 
    | (head_key, value)::t -> if List.mem head_key seen then
      let updatedRes = List.map (fun (k, v) -> if k = head_key then (k, v + value) else (k, v)) res in
      solve t updatedRes seen 
    else solve t ((head_key, value) :: res) (head_key :: seen) 
  in
  solve lst []  []

  