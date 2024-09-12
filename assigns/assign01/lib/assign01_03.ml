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
