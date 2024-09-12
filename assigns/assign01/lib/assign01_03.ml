(* Check if a number is prime *)
  let is_prime n =
    if n < 2 then
      false
    else
      let rec check i =
        if i >= n then
          true
        else if n mod i = 0 then
          false
        else
          check (i + 1)
      in
      check 2
  
  (* Find the nth prime number *)
let nth_prime n =
  let rec find_prime count num =
    if is_prime num then
      if count = n then num
      else find_prime (count + 1) (num + 1)
    else
      find_prime count (num + 1)
  in
  find_prime 0 2

  (* get exponent of the current prime in the factorization of s *)
let rec get_exponent s prime =
  if s mod prime = 0 then
     1 + get_exponent (s / prime) prime
  else 0

(* remove all factors of the current prime from s *)
let rec remove_prime_factors s prime =
  if s mod prime = 0 then 
    remove_prime_factors (s / prime) prime
  else s

(* Main function to find the i-th element in the sequence encoded by s *)
let nth s i =
  let rec find_element s curr_prime index =
    if index = i then get_exponent s curr_prime
    else
      let new_s = remove_prime_factors s curr_prime in
      find_element new_s (nth_prime (index + 1)) (index + 1)
  in
  find_element s (nth_prime 0) 0


