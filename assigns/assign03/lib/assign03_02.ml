
open Stdlib320
let rec sum lst =
  match lst with
  | [] -> 0  (* Base case: The sum of an empty list is 0 *)
  | x :: xs -> x + sum xs  (* Recursive case: sum the head with the sum of the tail *)
(*helper func to sum up the list*)

let gen_fib (genList : int list) k =
  let rec solve (newList : int list) numberOfTimes i =
    if i = numberOfTimes then sum newList 
    else if i < List.length newList then solve newList numberOfTimes (i + 1)
    else 
      let newSum = sum newList in
      solve (List.drop 1 newList @ [newSum]) numberOfTimes (i + 1)
  in
  solve genList k 0
