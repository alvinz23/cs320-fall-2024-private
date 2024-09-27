
open Stdlib320
let rec sum lst =
  match lst with
  | [] -> 0  
  | h :: t -> h + sum t  

  let gen_fib genList k =
    let lengthOfList = List.length genList in
    let rec getKth lst currentIndex =
      match lst with
      | [] -> 0 
      | h :: t -> if currentIndex = 0 then h else getKth t (currentIndex - 1)
    in
    if k < lengthOfList then
      getKth genList k
    else
      let rec gen i acc =
        match acc with
        | [] -> 0 
        | h :: _ ->
          if i = k then
            h  
          else
            let lastTerms = List.take lengthOfList acc in
            let sum = sum lastTerms in
            gen (i + 1) (sum :: acc)
      in
      gen (lengthOfList - 1) (List.rev genList)
  