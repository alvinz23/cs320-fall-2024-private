

open Stdlib320 

type matrix = {
  entries : float list list;
  rows : int;
  cols : int;
}

let mk_matrix (entries: float list) (r,c) : matrix =
  let rec buildRows current_entries remainingRows =
    if remainingRows <= 0 then []
    else
      let currentRow =
        if List.length current_entries < c then []
        else List.take c current_entries
      in
      let newEntries = List.drop c current_entries in
      let nextRows = buildRows newEntries (remainingRows - 1) in
      currentRow :: nextRows
  in

  let constructedRows = buildRows entries r in
  if entries = [] then { entries = []; rows = r; cols = c }
  else { entries = constructedRows; rows = r; cols = c }
