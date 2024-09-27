
let group l =
  let rec solve currList current_group groups =
    match currList with
    | [] ->
      let groups =
        if current_group = [] then groups
        else List.rev current_group :: groups
      in
      Some (List.rev groups)
    | h :: t ->
      if h = 0 then
        let groups =
          if current_group = [] then groups
          else List.rev current_group :: groups
        in
        solve t [] groups
      else
        solve t (h :: current_group) groups
  in
  solve l [] []
