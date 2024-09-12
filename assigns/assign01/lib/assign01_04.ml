
open Assign01_03
let to_string n =
  let rec constructString s i currStr =
    let num = nth s i in
    if num = 0 then 
      currStr
    else
      let newCurr = 
        if currStr = "" then 
          string_of_int num 
      else 
        currStr ^ "; " ^ string_of_int num in constructString s (i + 1) newCurr
  in
  "[" ^ constructString n 0 "" ^ "]"

(*Looked at Ocaml documents for string concatenation and manipulation*)