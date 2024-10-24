


open Utils

let lex s = 
  let words = split s in 
  let rec solve words acc = 
    match words with 
    | [] -> Some (List.rev acc)
    | h :: t -> (match tok_of_string_opt h with 
                | Some tok -> solve t (tok :: acc)
                | None -> None )
  in solve words [] 

