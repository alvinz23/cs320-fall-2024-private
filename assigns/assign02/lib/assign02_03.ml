

open Stdlib320

type dir = North | South | East | West
type path = dir list


let dist list = 
  let rec solve list a b = 
    let firstElement = 
      match list with 
      | h :: _ -> h
      | [] -> East
    in 
    if list <> [] then 
      if firstElement = North then 
        solve (List.drop(1) list) (a +. 1.0) b 
      else if firstElement = South then 
        solve (List.drop(1) list) (a -. 1.0) b 
      else if firstElement = East then 
        solve (List.drop(1) list) a (b +. 1.0)
      else if firstElement = West then 
        solve (List.drop(1) list) a (b -. 1.0)
      else 
        solve (List.drop(1) list) a b (* This branch ensures we keep going *)
    else 
      let aSquared = a *. a in
      let bSquared = b *. b in
      sqrt (aSquared +. bSquared)  (* Final return value when done *)
  in 
  solve list 0.0 0.0
