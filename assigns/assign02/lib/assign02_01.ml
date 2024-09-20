
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

  