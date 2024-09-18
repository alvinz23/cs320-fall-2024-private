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