
type set_info = {
  ind : int -> bool;
  mn : int;
  mx : int;
}

module ListSet = struct
  type t = int list
  let empty = []
  let mem someInt set = List.mem someInt set
  let singleton someInt = [someInt]
  let card set = List.length set
  let rec union set1 set2 =
    match set1, set2 with
    | [], s | s, [] -> s
    | head1 :: tail1, head2 :: tail2 ->
        if head1 = head2 then head1 :: union tail1 tail2 else
        if head1 < head2 then head1 :: union tail1 set2  else  head2 :: union set1 tail2
end

module FuncSet = struct
  type t = set_info

  let empty = { ind = (fun _ -> false); mn = 1; mx = 0 }

  let mem someInt set = set.ind someInt

  let singleton someInt = { ind = (fun x -> x = someInt); mn = someInt; mx = someInt }

  let card set =
    if set.mn > set.mx then 0
    else
      let rec countsomeInts currentsomeInt count =
        if currentsomeInt > set.mx then count
        else if set.ind currentsomeInt then countsomeInts (currentsomeInt + 1) (count + 1)
        else countsomeInts (currentsomeInt + 1) count
      in
      countsomeInts set.mn 0
  let union set1 set2 =
    let ind x = set1.ind x || set2.ind x in
    let mn =
      if set1.mn > set1.mx then set2.mn
      else if set2.mn > set2.mx then set1.mn
      else min set1.mn set2.mn
    in
    let mx =
      if set1.mn > set1.mx then set2.mx
      else if set2.mn > set2.mx then set1.mx
      else max set1.mx set2.mx
    in
    let rec adjustMin m =
      if m > mx then m + 1
      else if ind m then m
      else adjustMin (m + 1)
    in
    let rec adjustMax m =
      if m < mn then mn - 1
      else if ind m then m
      else adjustMax (m - 1)
    in
    let newMin = adjustMin mn in
    let newMax = adjustMax mx in
    { ind; mn = newMin; mx = newMax }
end
