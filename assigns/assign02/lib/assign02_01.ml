
type piece = 
  | X
  | O

type pos = 
  | Piece of piece
  | Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = 
  | Top
  | Middle
  | Bottom

type col_index = 
  | Left
  | Middle
  | Right

type pos_index = row_index * col_index

let get_pos (board : board) ((rowIndex, columnIndex) : pos_index) : pos =
  let extractRow (board : board) (rowIndex : row_index) : pos * pos * pos =
    let (top_row, middle_row, bottom_row) = board in
    match rowIndex with
    | Top -> top_row
    | Middle -> middle_row
    | Bottom -> bottom_row
  in

  let extractPos (row : pos * pos * pos) (columnIndex : col_index) : pos =
    let (leftPosition, middlePosition, rightPosition) = row in
    match columnIndex with
    | Left -> leftPosition
    | Middle -> middlePosition
    | Right -> rightPosition
  in

  let selectedRow = extractRow board rowIndex in
  let selectedPos = extractPos selectedRow columnIndex in
  selectedPos

  let allSamePieces p1 p2 p3 =
    match (p1, p2, p3) with
    | (Piece X, Piece X, Piece X) -> true
    | (Piece O, Piece O, Piece O) -> true
    | _ -> false
  
  let winner (board : board) : bool =
    let (row1, row2, row3) = board in
    
    let (p1, p2, p3) = row1 in
    let (p4, p5, p6) = row2 in
    let (p7, p8, p9) = row3 in
    
    let checkRows = 
      allSamePieces p1 p2 p3 || allSamePieces p4 p5 p6 || allSamePieces p7 p8 p9 
    in
    
    let checkColumns = 
      allSamePieces p1 p4 p7 || allSamePieces p2 p5 p8 || allSamePieces p3 p6 p9 
    in
  
    let checkDiagnol = 
      allSamePieces p1 p5 p9 || allSamePieces p3 p5 p7
    in
    
    if checkRows || checkColumns || checkDiagnol then
      true
    else
      false
  